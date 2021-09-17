/*
SPDX-License-Identifier: AGPL-3.0-or-later
Copyright (c) 2021 Szymon Rutkowski.
 */
package com.actualscan.crawling;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.apache.storm.spout.SpoutOutputCollector;
import org.apache.storm.topology.base.BaseRichSpout;
import org.apache.storm.topology.OutputFieldsDeclarer;
import org.apache.storm.task.TopologyContext;
import org.apache.storm.tuple.Fields;
import com.actualscan.crawling.DBQueryHandler;
import com.digitalpebble.stormcrawler.Metadata;

@SuppressWarnings("serial")
public class ScrapeRequestsSpout extends BaseRichSpout {
   protected boolean active;
   protected SpoutOutputCollector outputCollector;

   protected LinkedList<List<Object>> buffer = new LinkedList<>();

   private DBQueryHandler queryHandler;

   private static final Logger logger = LoggerFactory.getLogger(ScrapeRequestsSpout.class);

   public ScrapeRequestsSpout() {
   }

   @Override
   public void activate() {
      super.activate();
      active = true;
   }

   @Override
   public void deactivate() {
      super.deactivate();
      active = false;
   }

   @Override
   public void declareOutputFields(OutputFieldsDeclarer declarer) {
      declarer.declare(new Fields("url", "metadata"));
   }


   @Override
   public void open(@SuppressWarnings("rawtypes") Map conf,
         TopologyContext context, SpoutOutputCollector collector) {
      queryHandler = new DBQueryHandler(
            System.getenv("POSTGRES_HOST"),
            System.getenv("POSTGRES_PORT"),
            System.getenv("POSTGRES_USER"),
            System.getenv("POSTGRES_PASSWORD"));

      outputCollector = collector;

      // check that there is only one instance of it
      int totalTasks = context
         .getComponentTasks(context.getThisComponentId()).size();
      if (totalTasks > 1) {
         throw new RuntimeException(
               "Can't have more than one instance of the ScrapeRequestsSpout");
      }

      logger.info("Renewing scheduled and ran requests not committed...");
      try {
         queryHandler.executeUpdate("UPDATE scan_scraperequest SET status='waiting', status_changed=CURRENT_TIMESTAMP"
               +" WHERE status = 'scheduled' OR status = 'ran'");
      }
      catch (SQLException e) {
            logger.error("SQL error when trying to renew older requests", e);
      }
   }

   protected void fillBuffer() throws SQLException {
      // Cancel all the requests from terminated jobs.
      queryHandler.executeUpdate("UPDATE scan_scraperequest SET status='cancelled', status_changed=CURRENT_TIMESTAMP"
            + " WHERE job_id IN (SELECT id FROM scan_scanjob WHERE status = 'terminated') AND status != 'committed'");

      logger.debug("Getting a new set of scrape requests...");
      Connection conn = queryHandler.getConnection();
      Statement st = conn.createStatement();
      ResultSet scrapeRequestSet= st.executeQuery("SELECT r.id, r.target, r.job_id, r.is_search,"
            + " r.is_simple_crawl, r.site_id, r.site_name, r.site_url, r.source_type, r.query_tags,"
            + " r.lead_count, array_agg(t.name) site_tags"
            + " FROM scan_scraperequest AS r"
            + " LEFT JOIN scan_tagsitelink AS l ON r.site_id = l.site_id"
            + " LEFT JOIN scan_tag AS t ON l.tag_id = t.id"
            + " WHERE status = 'waiting' AND site_type = 'web'"
            + " GROUP BY r.id"
            + " ORDER BY is_search DESC LIMIT 50");

      while (scrapeRequestSet.next()) {
         Metadata metadata = new Metadata();
         metadata.addValue("req_id", String.valueOf(scrapeRequestSet.getInt("id")));
         metadata.addValue("job_id", String.valueOf(scrapeRequestSet.getInt("job_id")));
         metadata.addValue("site_id", String.valueOf(scrapeRequestSet.getInt("site_id")));
         metadata.addValue("is_search", String.valueOf(scrapeRequestSet.getBoolean("is_search")));
         metadata.addValue("is_crawl", String.valueOf(scrapeRequestSet.getBoolean("is_simple_crawl")));
         metadata.addValue("site_name", scrapeRequestSet.getString("site_name"));
         metadata.addValue("site_url", scrapeRequestSet.getString("site_url"));
         metadata.addValue("source_type", scrapeRequestSet.getString("source_type"));
         metadata.addValue("lead_count", String.valueOf(scrapeRequestSet.getInt("lead_count")));
         // Prefer query tags, use the site tags if there are none.
         Array query_tags = scrapeRequestSet.getArray("query_tags");
         if (((String[]) query_tags.getArray()).length > 0) {
            for (String tag : (String[]) query_tags.getArray()) {
               metadata.addValue("tags", tag);
            }
         }
         else {
            Array site_tags = scrapeRequestSet.getArray("site_tags");
            for (String tag : (String[]) site_tags.getArray()) {
               metadata.addValue("tags", tag);
            }
         }
         List<Object> nextRequest = new ArrayList<Object>();
         nextRequest.add(scrapeRequestSet.getString("target")); // the url field
         nextRequest.add(metadata);
         buffer.add(nextRequest);
      }
      conn.close();
   }

   @Override
   public void nextTuple() {
      if (!active)
         return;

      if (buffer.isEmpty()) {
         try {
            fillBuffer();
         }
         catch (SQLException e) {
            logger.info("Cannot connect to PostgreSQL for new scrape requests", e);
            return;
         }
      }
      if (buffer.isEmpty())
         return;

      List<Object> nextRequest = buffer.removeFirst();
      Metadata metadata = (Metadata) nextRequest.get(1);
      String reqId = metadata.getFirstValue("req_id");
      try {
         queryHandler.executeUpdate("UPDATE scan_scraperequest SET status='scheduled'"
               + " WHERE id = " + reqId);
      }
      catch (SQLException e) {
            logger.warn("Cannot update the status of a new scrape request "+reqId, e);
            return;
      }
      outputCollector.emit(nextRequest);
   }
}
