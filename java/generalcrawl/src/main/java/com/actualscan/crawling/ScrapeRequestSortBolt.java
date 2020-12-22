package com.actualscan.crawling;

import java.io.IOException;
import java.sql.*;
import java.util.Map;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.apache.storm.topology.OutputFieldsDeclarer;
import org.apache.storm.topology.base.BaseRichBolt;
import org.apache.storm.task.OutputCollector;
import org.apache.storm.task.TopologyContext;
import org.apache.storm.tuple.Fields;
import org.apache.storm.tuple.Tuple;
import org.apache.storm.tuple.Values;
import com.digitalpebble.stormcrawler.Metadata;

// The bolt that sorts fetched pages depending on whether they are and element of search or crawl.
@SuppressWarnings("serial")
public class ScrapeRequestSortBolt extends BaseRichBolt {

   private OutputCollector outputCollector;

   private DBQueryHandler queryHandler;

   private ResponseStringReader responseStringReader;

   private static final Logger logger = LogManager.getLogger(ScrapeRequestSortBolt.class);

   @Override
   public void declareOutputFields(OutputFieldsDeclarer declarer) {
      declarer.declareStream("search_reqs", new Fields("url", "content", "metadata"));
      declarer.declareStream("scan_leaf_reqs", new Fields("url", "content", "metadata"));
      declarer.declareStream("crawl_reqs", new Fields("url", "content", "metadata"));
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

      outputCollector = collector;
      responseStringReader = new ResponseStringReader();
   }

   @Override
   public void execute(Tuple tuple) {
      Metadata metadata = null;

      if (tuple.contains("metadata"))
         metadata = (Metadata) tuple.getValueByField("metadata");

      // TODO drop? if no metadata

      String url = (String) tuple.getValueByField("url");
      byte[] content = (byte[]) tuple.getValueByField("content");

      try {
         String contentString = responseStringReader.responseText(content, url, metadata);
         // Sort the requests between further bolts.
         if (metadata.getFirstValue("is_crawl") == "true") {
            outputCollector.emit("crawl_reqs", tuple, new Values(url, contentString, metadata));
         }
         else if (metadata.getFirstValue("is_search") == "true") {
            outputCollector.emit("search_reqs", tuple, new Values(url, contentString, metadata));
         }
         else {
            outputCollector.emit("scan_leaf_reqs", tuple, new Values(url, contentString, metadata));
         }

         String reqId = metadata.getFirstValue("req_id");
         try {
            queryHandler.executeUpdate("UPDATE scan_scraperequest SET status='ran', status_changed=CURRENT_TIMESTAMP"
                  + " WHERE id = " + reqId);
         }
         catch (SQLException e) {
            logger.warn("Cannot update the status of a new scrape request "+reqId, e);
            return;
         }
         outputCollector.ack(tuple);
      }
      catch (IOException e) {
         logger.error(e);
         outputCollector.fail(tuple);
      }
   }
}
