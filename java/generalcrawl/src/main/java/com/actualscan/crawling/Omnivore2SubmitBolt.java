package com.actualscan.crawling;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.sql.*;
import java.time.Duration;
import java.time.format.DateTimeFormatter;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Base64;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import javax.net.ssl.SSLContext;

import org.apache.http.Consts;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.entity.StringEntity;
import org.apache.http.NameValuePair;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.message.BasicNameValuePair;
import org.apache.storm.topology.OutputFieldsDeclarer;
import org.apache.storm.topology.base.BaseRichBolt;
import org.apache.storm.task.OutputCollector;
import org.apache.storm.task.TopologyContext;
import org.apache.storm.tuple.Fields;
import org.apache.storm.tuple.Tuple;
import org.apache.storm.tuple.Values;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.digitalpebble.stormcrawler.Metadata;

@SuppressWarnings("serial")
public class Omnivore2SubmitBolt extends BaseRichBolt {

   private OutputCollector outputCollector;

   private DBQueryHandler queryHandler;
   private SSLConnectionSocketFactory sslSocketFactory;
   private String speechtractorAuthString;
   private String speechtractorURIString;
   private String omnivore2URIString;

   private static final Logger logger = LogManager.getLogger(Omnivore2SubmitBolt.class);

   @Override
   public void declareOutputFields(OutputFieldsDeclarer declarer) {
      declarer.declare(new Fields("url", "content", "metadata"));
   }

   @SuppressWarnings("rawtypes")
   @Override
   public void prepare(Map conf, TopologyContext context,
         OutputCollector collector) {
      queryHandler = new DBQueryHandler(
            System.getenv("POSTGRES_HOST"),
            System.getenv("POSTGRES_PORT"),
            System.getenv("POSTGRES_USER"),
            System.getenv("POSTGRES_PASSWORD"));

      try {
         SSLContext sslContext = new SSLContextFactory(
               System.getenv("KEYSTORE_PATH"),
               System.getenv("KEYSTORE_PASSWORD")).get();
         sslSocketFactory = new SSLConnectionSocketFactory(
               sslContext,
               new String[]{"TLSv1.2"}, null,    
               SSLConnectionSocketFactory.getDefaultHostnameVerifier());
      }
      catch (Exception e) {
            logger.error("Cannot initialize a SSLContext", e);
      }

      outputCollector = collector;

      speechtractorAuthString = "Basic " + Base64.getEncoder().encodeToString(
            (System.getenv("SPEECHTRACTOR_USER")+":"+System.getenv("SPEECHTRACTOR_PASSWORD"))
            .getBytes());
      speechtractorURIString = "https://" + System.getenv("SPEECHTRACTOR_HOST")
         + ":" + System.getenv("SPEECHTRACTOR_PORT")
         + "/api/v01/interpret";
      omnivore2URIString = "https://" + System.getenv("OMNIVORE2_HOST")
         + ":" + System.getenv("OMNIVORE2_PORT")
         + "/eat/";
   }

   @Override
   public void execute(Tuple tuple) {
      String url = (String) tuple.getValueByField("url");
      String content = tuple.getStringByField("content");
      Metadata metadata = (Metadata) tuple.getValueByField("metadata");

      //
      // Get the Speechtractor's reading of the text.
      // 
      // Determine info for the POST form params.
      String sourceType = metadata.getFirstValue("source_type");
      String emptyUrl = "";
      if (sourceType == "media" || sourceType == "blog")
         emptyUrl = "1";
      else
         emptyUrl = "0";

      // Constructing and sending the request.
      HttpClientBuilder clientBouilder = HttpClients.custom();
      clientBouilder = clientBouilder.setSSLSocketFactory(sslSocketFactory);
      CloseableHttpClient stractorClient = clientBouilder.build();
      HttpPost stractorRequest = new HttpPost(speechtractorURIString);
      // The POST form params.
      List<NameValuePair> form = new ArrayList<>();
      form.add(new BasicNameValuePair("html", URLEncoder.encode(content)));
      form.add(new BasicNameValuePair("sourcetype", sourceType));
      form.add(new BasicNameValuePair("emptyurl", emptyUrl));
      UrlEncodedFormEntity formEntity = new UrlEncodedFormEntity(form, Consts.UTF_8);
      stractorRequest.setEntity(formEntity);
      stractorRequest.addHeader("Content-type", "application/x-www-form-urlencoded");
      stractorRequest.addHeader("Accept", "text/json");
      stractorRequest.addHeader("Authorization", speechtractorAuthString);
      String stage = "pre-stractor";
      try {
         HttpResponse stractorResponse = stractorClient.execute(stractorRequest);
         if (stractorResponse.getStatusLine().getStatusCode() != 200) {
            outputCollector.fail(tuple);
            logger.error("Cannot send documents to Speechtractor for "+url
                  +", status code: "+Integer.toString((stractorResponse.getStatusLine().getStatusCode())));
            return;
         }
         stage = "got-stractor";

         // Parsing JSON and filling missing attributes in the representation.
         ObjectMapper stractorMapper = new ObjectMapper();
         stractorMapper.setPropertyNamingStrategy(PropertyNamingStrategy.SNAKE_CASE);
         List<StractorDocument> stractorDocs = stractorMapper.readValue(
               EntityUtils.toString(stractorResponse.getEntity()),
               new TypeReference<List<StractorDocument>>(){});
         stractorClient.close();
         String timestamp = ZonedDateTime.now(ZoneOffset.UTC).format(DateTimeFormatter.ISO_INSTANT);
         for (StractorDocument doc : stractorDocs) {
            doc.siteName = metadata.getFirstValue("site_name");
            doc.dateRetr = timestamp;
            doc.tags = metadata.getValues("doc_tags"); // taken from query or site tags

            if (sourceType == "media" || sourceType == "blog") {
               doc.realDoc = "self";
               doc.url = url;
            }
            else
               doc.realDoc = url;
         }
         String readyDocsString = stractorMapper.writeValueAsString(stractorDocs);

         // Send the documents to Omnivore2.
         CloseableHttpClient omniv2Client = clientBouilder.build();
         HttpPost omniv2Request = new HttpPost(omnivore2URIString);
         StringEntity omniv2Entity = new StringEntity(readyDocsString);
         omniv2Request.setEntity(omniv2Entity);
         omniv2Request.addHeader("Content-type", "application/json");
         HttpResponse omniv2Response = omniv2Client.execute(omniv2Request);
         if (omniv2Response.getStatusLine().getStatusCode() != 200) {
            outputCollector.fail(tuple);
            logger.error("Cannot send documents to Omnivore2 for "+url
                  +", status code: "+Integer.toString((omniv2Response.getStatusLine().getStatusCode())));
            return;
         }
         stage = "omniv2-sent";
         omniv2Client.close();

         String reqId = metadata.getFirstValue("req_id");
         try {
            queryHandler.executeUpdate("UPDATE scan_scraperequest SET status='committed', status_changed=CURRENT_TIMESTAMP"
                  + " WHERE id = " + reqId);
         }
         catch (SQLException e) {
            logger.warn("Cannot update the status of a new scrape request "+reqId, e);
            return;
         }
         outputCollector.emit(tuple, new Values(url, content, metadata));
         outputCollector.ack(tuple);
      }
      catch (Exception e) {
            outputCollector.fail(tuple);
            logger.error("Cannot do standard page processing for "+url+" at stage "+stage,
                  e);
            return;
      }
   }
}
