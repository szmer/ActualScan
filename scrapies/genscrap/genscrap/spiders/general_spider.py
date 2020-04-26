from datetime import datetime, timezone
import json
from logging import info
import os, os.path

import simpleflock

import scrapy
from sqlalchemy.ext.automap import automap_base
from sqlalchemy.orm import Session
from sqlalchemy import create_engine

from genscrap.flask_instance.settings import SQLALCHEMY_DATABASE_URI 
from genscrap.lib import (
        write_to_file, stractor_reading, timestamp_now, full_url, solr_update, site_id_tags,
        update_request_status
        )

FILEDB_PATH = ''
# Note it's the original port, not the one mapped by docker-compose.
SOLR_NETLOC = 'solr:8983'
SOLR_PING_URL = 'http://solr:8983/solr/lookupy/admin/ping'
REQUEST_META = ['id', 'is_search', 'job_id', 'query_tags', 'save_copies', 'site_name', 'site_url',
        'site_id', 'source_type']

#
# Postgres connection & ORM setup.
#
AutomapBase = automap_base()
pg_engine = create_engine(SQLALCHEMY_DATABASE_URI)
# Reflect the tables.
AutomapBase.prepare(pg_engine, reflect=True)

ScrapeRequest = AutomapBase.classes.scrape_request
Site = AutomapBase.classes.site
pg_session = Session(pg_engine)

#
# The spider.
#

class GeneralSpider(scrapy.Spider):
    name = 'general'
    # We want to catch them to notify the DB of failure.
    handle_httpstatus_list = [400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413,
            414, 415, 416, 417, 500, 501, 502, 503, 504, 505]

    def start_requests(self):
        yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                errback=self.fail)

    def fail(self, failure):
        """
        This is to be called as errback for Scrapy requests to log HTTP, DNS errors etc.
        """
        self.logger.error(repr(failure))

    def save_page_to_disk(self, db_path, url, page_html, meta):
        """
        Save the page to disk for manual inspection.
        """
        with simpleflock.SimpleFlock(db_path+'index.csv.lock'):
            if os.path.isfile(db_path+'index.csv'):
                with open(db_path+'index.csv', 'r') as db_index_file:
                    entry_n = len(db_index_file.readlines())
            else:
                entry_n = 0
            db_catalog = db_path + str(entry_n) + '/'
            os.makedirs(db_catalog, exist_ok=False)
            write_to_file(db_catalog+'index', page_html)
            write_to_file(db_catalog+'url', url)
            write_to_file(db_catalog+'access-timestamp', timestamp_now())
            for key, value in meta.items():
                write_to_file(db_catalog+key, value)
            # Append the db entry for this page.
            with open(db_path+'index.csv', 'a+') as db_index_file:
                print(entry_n, url, file=db_index_file)

    def monitoring_parse(self, response):
        """
        Scrapy parse function for Solr dummy monitoring requests - we want to search Postgres for
        new requests.
        """
        scrape_requests_waiting = list(pg_session.query(ScrapeRequest).filter_by(
            status='waiting', site_type='web'))
        requests_done = False
        for scrape_request in scrape_requests_waiting:
            update_request_status(pg_session, scrape_request, 'scheduled')
            meta = {attr: getattr(scrape_request, attr)
                        for attr
                        in REQUEST_META}
            url = full_url(scrape_request.target, scrape_request.site_url)
            yield scrapy.Request(url=url, callback=self.parse, meta=meta, errback=self.fail,
                    priority=5 if 'is_search' in meta and meta['is_search'] else 0)
            requests_done = True
        if not requests_done:
            yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                    errback=self.fail)

    def interpret_stractor(self, source_type, html_text, request_id):
        """
        Interpret the html_text on source_type with Speechtractor. Indicate failure in request_id.
        Return the object loaded from response JSON.
        """
        stractor_output = stractor_reading(html_text, source_type)
        # Speechtractor failures.
        if isinstance(stractor_output, int):
            update_request_status(pg_session,
                    pg_session.query(ScrapeRequest).get(request_id),
                    'failed',
                    failure_comment='Could not reach Speechtractor, status code {}'.format(
                        stractor_output))
        return json.loads(stractor_output)

    def status_notify_db(self, db_request, status):
        """
        If status is not HTTP 200 (OK), mark the request of the id as failed. Return a boolean
        indicating if the status is OK.
        """
        # Notify of failures if needed.
        if status != 200:
            update_request_status(pg_session, db_request, 'failed',
                    failure_comment='Status code: {}'.format(status))
        else:
            update_request_status(pg_session, db_request, 'ran')
        if status != 200:
            return False
        return True

    def parse(self, response):
        """
        The "regular" Scrapy parse function for search pages and indexed pages. The former should
        yield some new Scrapy Requests to follow.
        """
        db_request = pg_session.query(ScrapeRequest).get(response.meta['id'])
        # Update the status of the request (ran, failed).
        ok = self.status_notify_db(db_request, response.status)
        if not ok:
            return

        # Check if a new monitoring request is necessary. TODO check jobs instead maybe?
        scrape_requests_waiting_count = pg_session.query(ScrapeRequest).filter_by(
            status='waiting', site_type='web').count()
        if scrape_requests_waiting_count == 0:
            yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                    errback=self.fail)

        # Interpret the webpage with speechtractor.
        if response.meta['is_search']:
           source_type = 'searchpage'
        else:
            source_type = response.meta['source_type']
        # may cause JSONDecodeError
        stractor_response_json = self.interpret_stractor(source_type, response.text,
                response.meta['id'])

        # Handling search pages.
        if response.meta['is_search']:
            if stractor_response_json is not None and stractor_response_json:
                for found_page in stractor_response_json:
                    if 'url' in found_page:
                        is_page_search = (True
                                if 'is_search' in found_page and found_page['is_search']
                                else False)
                        scrape_request = ScrapeRequest(target=found_page['url'],
                                is_search=is_page_search,
                                job_id=response.meta['job_id'],
                                status='scheduled',
                                status_changed=datetime.now(timezone.utc),
                                source_type=response.meta['source_type'],
                                query_tags=response.meta['query_tags'],
                                site_name=response.meta['site_name'],
                                site_type='web',
                                site_url=response.meta['site_url'],
                                site_id=response.meta['site_id'],
                                save_copies=response.meta['save_copies'])
                        info('Created a scrape request for {} from search, for {}'.format(
                            found_page['url'], response.meta['job_id']))
                        pg_session.add(scrape_request)
                        pg_session.commit()
                        yield response.follow(found_page['url'],
                                callback=self.parse,
                                meta={attr: getattr(scrape_request, attr) for attr
                                    in REQUEST_META},
                                priority=5 if is_page_search else 0)
                update_request_status(pg_session, db_request, 'committed')
            else:
                update_request_status(pg_session, db_request, 'failed',
                        failure_comment='empty Speechtractor response')
                info('Got empty Speechractor response: {} for search target: {}'.format(
                    stractor_response_json, response.url))
        # The actual page indexing.
        else:
            site_tags = site_id_tags(response.meta['site_id'], pg_session)
            if stractor_response_json is not None and stractor_response_json:
                for doc in stractor_response_json:
                    doc['date_retr'] = timestamp_now()
                    doc['site_name'] = response.meta['site_name']
                    doc['source_type'] = response.meta['source_type']
                    if len(stractor_response_json) == 1:
                        doc['real_doc'] = 'self'
                        doc['url'] = response.url
                    else:
                        doc['real_doc'] = response.url
                    doc['url'] = full_url(doc['url'], response.meta['site_url'])
                    doc['tags'] = site_tags,
                    doc['reason_scraped'] = response.meta['job_id']
                # This sends documents to solr with default settings. We need to do a manual commit
                # afterwards.
                solr_json_text = json.dumps(stractor_response_json)
                solr_update(solr_json_text, req_id=response.meta['id'],
                        req_class=ScrapeRequest, pg_session=pg_session)
                # Do the manual commit.
                # TODO commit one doc in one go with {add: {doc:}} json structure
                solr_update('{"commit": {}}', req_id=response.meta['id'],
                        req_class=ScrapeRequest, pg_session=pg_session)
                update_request_status(pg_session, db_request, 'committed')
            else:
                update_request_status(pg_session, db_request, 'failed',
                        failure_comment='empty Speechtractor response')
                info('Got empty Speechractor response: {} for scraping target: {}'.format(
                    stractor_response_json, response.url))

        if response.meta['save_copies']:
            # Save the page.
            self.save_page_to_disk(FILEDB_PATH, response.url, response.text, response.meta)
