package com.actualscan.crawling;

/**
 * Licensed to DigitalPebble Ltd under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * DigitalPebble licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/* Modified for ActualScan by Szymon Rutkowski. */

import com.digitalpebble.stormcrawler.*;
import com.digitalpebble.stormcrawler.bolt.FetcherBolt;
import com.digitalpebble.stormcrawler.bolt.JSoupParserBolt;
import com.digitalpebble.stormcrawler.bolt.SiteMapParserBolt;
import com.digitalpebble.stormcrawler.bolt.URLPartitionerBolt;
import com.digitalpebble.stormcrawler.bolt.FeedParserBolt;
import com.digitalpebble.stormcrawler.indexing.StdOutIndexer;
import com.digitalpebble.stormcrawler.persistence.StdOutStatusUpdater;

import org.apache.storm.Config;
import org.apache.storm.topology.TopologyBuilder;
import org.apache.storm.tuple.Fields;

public class CrawlTopology extends ConfigurableTopology {
    public static void main(String[] args) throws Exception {
       // the first four command-line argumets specify: host, port, username, password
       ConfigurableTopology.start(new CrawlTopology(), args);
    }

    @Override
    protected int run(String[] args) {
        TopologyBuilder builder = new TopologyBuilder();

        builder.setSpout("spout", new ScrapeRequestsSpout());

        builder.setBolt("partitioner", new URLPartitionerBolt())
                .shuffleGrouping("spout");

        // TODO !!! filters (depth etc.) must be declared in storm config
        builder.setBolt("fetch", new RobotsIgnoringFetcherBolt())
                .fieldsGrouping("partitioner", new Fields("key")); // the key was given looking at the domain

// TODO We could use these to parse sitemaps and feeds in simple crawls, but need to figure out
// the metadata to control this
///-        builder.setBolt("sitemap", new SiteMapParserBolt())
///-                .localOrShuffleGrouping("fetch");
///-
///-        builder.setBolt("feeds", new FeedParserBolt())
///-                .localOrShuffleGrouping("sitemap");

        builder.setBolt("sort_reqs", new ScrapeRequestSortBolt())
                .localOrShuffleGrouping("fetch");

        builder.setBolt("submit_to_omnivore2", new Omnivore2SubmitBolt())
                .localOrShuffleGrouping("sort_reqs", "scan_leaf_reqs");

        builder.setBolt("process_search_pages", new SearchPageRequestsBolt())
                .localOrShuffleGrouping("sort_reqs", "search_reqs");

        Fields furl = new Fields("url");
        // can also use MemoryStatusUpdater for simple recursive crawls
        builder.setBolt("status", new StdOutStatusUpdater())
                .fieldsGrouping("fetch", Constants.StatusStreamName, furl);

        return submit("crawl", conf, builder);
    }
}
