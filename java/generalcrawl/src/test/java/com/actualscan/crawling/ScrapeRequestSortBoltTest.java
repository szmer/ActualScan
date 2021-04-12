/*
SPDX-License-Identifier: AGPL-3.0-or-later
Copyright (c) 2021 Szymon Rutkowski.
 */
package com.actualscan.crawling;

import com.digitalpebble.stormcrawler.Metadata;
import org.apache.storm.task.OutputCollector;
import org.apache.storm.task.TopologyContext;
import org.apache.storm.tuple.Fields;
import org.apache.storm.tuple.TupleImpl;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ScrapeRequestSortBoltTest {
   MockOutputCollector implemOutputCollector = new MockOutputCollector();
   OutputCollector testOutputCollector = new OutputCollector(implemOutputCollector);
   ScrapeRequestSortBolt testBolt = new ScrapeRequestSortBolt();
   @Mock
   private TopologyContext testTopologyContext = mock(TopologyContext.class);;
   HashMap<String, String> testStormConfig = new HashMap<String,String>();;

   @BeforeEach
   public void prepare() {
      testBolt.prepare(testStormConfig, testTopologyContext, testOutputCollector);
      // TupleImpl constructor wants to use these methods
      when(testTopologyContext.getComponentId(anyInt())).thenReturn("-1");
      when(testTopologyContext.getComponentOutputFields(anyString(), anyString())).thenReturn(
              new Fields("url", "content", "metadata"));
      implemOutputCollector.emittedTuples.clear();
   }

   @Test
   public void testSortingSearchRequest() {
      Metadata searchReqMetadata = new Metadata();
      // An unexisting scrape request, nothing will happen to it
      // TODO test what should actually happen to db rows
      searchReqMetadata.addValue("req_id", "-1");
      searchReqMetadata.addValue("is_search", "true");
      searchReqMetadata.addValue("is_crawl", "false");
      searchReqMetadata.addValue("content-type", "text/html");
      List<Object> searchReqTuple = new ArrayList<Object>();
      searchReqTuple.add("http://books.toscrape.com");
      searchReqTuple.add("<html>test</html>");
      searchReqTuple.add(searchReqMetadata);

      testBolt.execute(new TupleImpl(testTopologyContext,
               searchReqTuple, 0, "default"));
      assertNotNull(implemOutputCollector.emittedTuples.get("search_reqs"));
      assertNull(implemOutputCollector.emittedTuples.get("scan_leaf_reqs"));
      assertNull(implemOutputCollector.emittedTuples.get("crawl_reqs"));
   }

   @Test
   public void testSortingLeafRequest() {
      Metadata leafReqMetadata = new Metadata();
      // An unexisting scrape request, nothing will happen to it
      // TODO test what should actually happen to db rows
      leafReqMetadata.addValue("req_id", "-1");
      leafReqMetadata.addValue("is_search", "false");
      leafReqMetadata.addValue("is_crawl", "false");
      List<Object> leafReqTuple = new ArrayList<Object>();
      leafReqTuple.add("http://books.toscrape.com");
      leafReqTuple.add("<html>test</html>");
      leafReqTuple.add(leafReqMetadata);

      testBolt.execute(new TupleImpl(testTopologyContext,
               leafReqTuple, 0, "default"));
      assertNotNull(implemOutputCollector.emittedTuples.get("scan_leaf_reqs"));
      assertNull(implemOutputCollector.emittedTuples.get("search_reqs"));
      assertNull(implemOutputCollector.emittedTuples.get("crawl_reqs"));
   }

   @Test
   public void testSortingCrawlRequest() {
      Metadata crawlReqMetadata = new Metadata();
      // An unexisting scrape request, nothing will happen to it
      // TODO test what should actually happen to db rows
      crawlReqMetadata.addValue("req_id", "-1");
      crawlReqMetadata.addValue("is_search", "false");
      crawlReqMetadata.addValue("is_crawl", "true");
      List<Object> crawlReqTuple = new ArrayList<Object>();
      crawlReqTuple.add("http://books.toscrape.com");
      crawlReqTuple.add("<html>test</html>");
      crawlReqTuple.add(crawlReqMetadata);

      testBolt.execute(new TupleImpl(testTopologyContext,
               crawlReqTuple, 0, "default"));
      assertNotNull(implemOutputCollector.emittedTuples.get("crawl_reqs"));
      assertNull(implemOutputCollector.emittedTuples.get("scan_leaf_reqs"));
      assertNull(implemOutputCollector.emittedTuples.get("search_reqs"));
   }
}
