package com.actualscan.crawling;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.sql.*;
import java.time.Duration;
import java.util.Base64;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.net.ssl.SSLContext;

import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
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
import com.digitalpebble.stormcrawler.util.URLUtil;

@SuppressWarnings("serial")
public class SearchPageRequestsBolt extends BaseRichBolt {

   private OutputCollector outputCollector;

   private DBQueryHandler queryHandler;
   private SSLConnectionSocketFactory sslSocketFactory;
   private String speechtractorAuthString;
   private String speechtractorURIString;

   private int maxNormalSearchDepth;

   private static final Logger logger = LogManager.getLogger(SearchPageRequestsBolt.class);


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

      maxNormalSearchDepth = 2;//System.getenv("scanning.normal_search_depth");

      speechtractorAuthString = "Basic " + Base64.getEncoder().encodeToString(
            (System.getenv("SPEECHTRACTOR_USER")+":"+System.getenv("SPEECHTRACTOR_PASSWORD"))
            .getBytes());
      speechtractorURIString = "https://" + System.getenv("SPEECHTRACTOR_HOST")
         + ":" + System.getenv("SPEECHTRACTOR_PORT")
         + "/api/v01/interpret";
   }

   @Override
   public void execute(Tuple tuple) {
      String url = (String) tuple.getValueByField("url");
      String content = tuple.getStringByField("content");
      Metadata metadata = (Metadata) tuple.getValueByField("metadata");

      //
      // Get the Speechtractor's reading of the text.
      // 
      // Constructing and sending the request.
      HttpClientBuilder clientBouilder = HttpClients.custom();
      clientBouilder = clientBouilder.setSSLSocketFactory(sslSocketFactory);
      CloseableHttpClient stractorClient = clientBouilder.build();
      HttpPost stractorRequest = new HttpPost(speechtractorURIString);
      // The POST form params.
      List<NameValuePair> form = new ArrayList<>();
      form.add(new BasicNameValuePair("html", content));
      form.add(new BasicNameValuePair("sourcetype", "searchpage"));
      form.add(new BasicNameValuePair("emptyurl", "0"));
      UrlEncodedFormEntity formEntity = new UrlEncodedFormEntity(form, Consts.UTF_8);
      stractorRequest.setEntity(formEntity);
      stractorRequest.addHeader("Content-type", "application/x-www-form-urlencoded");
      stractorRequest.addHeader("Accept", "text/json");
      stractorRequest.addHeader("Authorization", speechtractorAuthString);
      try {
         HttpResponse stractorResponse = stractorClient.execute(stractorRequest);
         if (stractorResponse.getStatusLine().getStatusCode() != 200) {
            outputCollector.fail(tuple);
            logger.error("Cannot send documents to Speechtractor for "+url
                  +", status code: "+Integer.toString((stractorResponse.getStatusLine().getStatusCode())));
            return;
         }
         String responseText = EntityUtils.toString(stractorResponse.getEntity());
         ObjectMapper stractorMapper = new ObjectMapper();
         stractorMapper.setPropertyNamingStrategy(PropertyNamingStrategy.SNAKE_CASE);
         List<StractorDocument> stractorDocs = stractorMapper.readValue(
               responseText,
               new TypeReference<List<StractorDocument>>(){});
         stractorClient.close();
         URL sourceURL = new URL(metadata.getFirstValue("site_url"));
         for (StractorDocument doc : stractorDocs) {
            if (doc.isSearch // drop search beyond the designated depth
                  && Integer.parseInt(metadata.getFirstValue("lead_count"))
                  >= maxNormalSearchDepth)
               continue;

            // Get the canonical URL.
            URL targetURL = URLUtil.resolveURL(sourceURL, doc.url);

            Connection conn = queryHandler.getConnection();
            PreparedStatement insertStmt = conn.prepareStatement(
                  "INSERT INTO scan_scraperequest"
                  + "(target, is_search, is_simple_crawl, job_id, lead_count, status, source_type, site_name,"
                  + " site_type, site_url, site_id, status_changed, query_tags)"
                  + " VALUES (?, ?, FALSE, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP,"
                  // Get the tags for the job (preferably) or the tags for the site
                  + " (SELECT coalesce(nullif('{}', (SELECT query_tags FROM scan_scanjob where id = ?)),"
                  + "                  (SELECT array(SELECT name FROM scan_tag AS t"
                  + "                                WHERE t.id IN (SELECT tag_id FROM scan_tagsitelink"
                  + "                                               WHERE site_id = ?))))))"
                  );
            insertStmt.setString(1, targetURL.toString());
            insertStmt.setBoolean(2, doc.isSearch);
            insertStmt.setInt(3, Integer.parseInt(metadata.getFirstValue("job_id")));
            insertStmt.setInt(4, doc.isSearch ?
                  (1+Integer.parseInt(metadata.getFirstValue("lead_count")))
                  : 0);
            insertStmt.setString(5, "waiting"); // status
            insertStmt.setString(6, metadata.getFirstValue("source_type")); // needs to be original type even for search
            insertStmt.setString(7, metadata.getFirstValue("site_name"));
            insertStmt.setString(8, "web"); // site_type
            insertStmt.setString(9,  metadata.getFirstValue("site_url"));
            insertStmt.setInt(10, Integer.parseInt(metadata.getFirstValue("site_id")));
            insertStmt.setInt(11, Integer.parseInt(metadata.getFirstValue("job_id")));
            insertStmt.setInt(12, Integer.parseInt(metadata.getFirstValue("site_id")));
            insertStmt.executeUpdate();
            conn.close();
         }

         String reqId = metadata.getFirstValue("req_id");
         try {
            // The search page is now committed.
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
            logger.error("Cannot do search page processing for "+url, e);
            return;
      }
   }
}
