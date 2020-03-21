from datetime import datetime, timezone
import http.client
import json
import os, os.path

import simpleflock

import scrapy
from sqlalchemy.ext.automap import automap_base
from sqlalchemy.orm import Session
from sqlalchemy import create_engine

from genscrap.flask_instance.settings import SQLALCHEMY_DATABASE_URI 
from genscrap.lib import write_to_file, stractor_reading, timestamp_now, full_url

FILEDB_PATH = ''
# Note it's the original port, not the one mapped by docker-compose.
SOLR_NETLOC = 'solr:8983'
SOLR_PING_URL = 'http://solr:8983/solr/lookupy/admin/ping'
REQUEST_META = ['id', 'is_search', 'job_id', 'query_tags', 'save_copies', 'site_name', 'site_url',
        'source_type']

#
# Postgres connection & ORM setup.
#
AutomapBase = automap_base()
pg_engine = create_engine(SQLALCHEMY_DATABASE_URI)
# Reflect the tables.
AutomapBase.prepare(pg_engine, reflect=True)

ScrapeRequest = AutomapBase.classes.scrap_request
pg_session = Session(pg_engine)

#
# The spider.
#

class GeneralSpider(scrapy.Spider):
    name = 'general'
    # We want to catch them to notify the DB of failure.
    handle_httpstatus_list = [400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413,
            414, 415, 416, 417, 500, 501, 502, 503, 504, 505]

    def __init__(self):
        self.solr_conn = http.client.HTTPConnection('solr', port=8983, timeout=10)

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

    def solr_update(self, req_body, req_id=None):
        """
        Send req_body as payload to Solr, optionally updating the req_id ScrapeRequest on failure.
        Return a boolean indicating success or failure.
        """
        self.logger.debug('Sent to Solr: {}'.format(req_body))
        self.solr_conn.request('GET', '/solr/lookupy/update', body=req_body,
            headers={'Content-type': 'application/json'})
        solr_response = self.solr_conn.getresponse()
        self.logger.debug('Solr response: {}'.format(solr_response.status))
        if solr_response.status != 200:
            if req_id is not None:
                db_request = pg_session.query(ScrapeRequest).get(req_id)
                db_request.status = 'failed'
                db_request.failure_comment = 'Could not reach Solr, status code {}'.format(
                        solr_response.status) 
                pg_session.commit()
            return False
        return True

    def monitoring_parse(self, response):
        """
        Scrapy parse function for Solr dummy monitoring requests - we want to search Postgres for
        new requests.
        """
        scrap_requests_waiting = list(pg_session.query(ScrapeRequest).filter_by(
            status='waiting'))
        requests_done = False
        for scrap_request in scrap_requests_waiting:
            scrap_request.status = 'scheduled'
            meta = {attr: getattr(scrap_request, attr)
                        for attr
                        in REQUEST_META}
            pg_session.commit()
            url = full_url(scrap_request.url, scrap_request.site_url)
            yield scrapy.Request(url=url, callback=self.parse, meta=meta, errback=self.fail)
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
            db_request = pg_session.query(ScrapeRequest).get(request_id)
            db_request.status = 'failed'
            db_request.failure_comment = 'Could not reach Speechtractor, status code {}'.format(
                    stractor_output) 
            pg_session.commit()
        return json.loads(stractor_output)

    def status_notify_db(self, request_id, status):
        """
        If status is not HTTP 200 (OK), mark the request of the id as failed. Return a boolean
        indicating if the status is OK.
        """
        db_request = pg_session.query(ScrapeRequest).get(request_id)
        db_request.status_changed = datetime.now(timezone.utc)
        # Notify of failures if needed.
        if status != 200:
            db_request.status = 'failed'
            db_request.failure_comment = 'Status code: {}'.format(status)
        else:
            db_request.status = 'ran'
        pg_session.commit()
        if status != 200:
            return False
        return True

    def parse(self, response):
        """
        The "regular" Scrapy parse function for search pages and indexed pages. The former should
        yield some new Scrapy Requests to follow.
        """
        # Update the status of non-monitoring requests.
        ok = self.status_notify_db(response.meta['id'], response.status)
        if not ok:
            return

        # Check if a new monitoring request is necessary.
        scrap_requests_waiting_count = pg_session.query(ScrapeRequest).filter_by(
            status='waiting').count()
        if scrap_requests_waiting_count == 0:
            yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                    errback=self.fail)

        # Interpret the website with speechtractor.
        if response.meta['is_search']:
           source_type = 'searchpage'
        else:
            source_type = response.meta['source_type']
        stractor_response_json = self.interpret_stractor(source_type, response.text,
                response.meta['id'])

        # Handling search pages.
        if response.meta['is_search']:
            # TODO go to the next page!
            # may cause JSONDecodeError
            for found_page in stractor_response_json:
                if 'url' in found_page:
                    scrap_request = ScrapeRequest(url=found_page['url'], is_search=False,
                            job_id=response.meta['job_id'], status='waiting',
                            status_changed=datetime.now(timezone.utc),
                            source_type=response.meta['source_type'],
                            query_tags=response.meta['query_tags'],
                            site_name=response.meta['site_name'],
                            site_url=response.meta['site_url'],
                            save_copies=response.meta['save_copies'])
                    pg_session.add(scrap_request)
                    pg_session.commit()
                    yield response.follow(found_page['url'],
                            callback=self.parse,
                            meta={attr: getattr(scrap_request, attr) for attr
                                in REQUEST_META})
        # The actual page indexing.
        else:
            # may cause JSONDecodeError
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
                doc['tags'] = response.meta['query_tags'].split(',')
                doc['reason_scraped'] = response.meta['job_id']
            # This sends documents to solr with default settings. We need to do a manual commit
            # afterwards.
            solr_json_text = json.dumps(stractor_response_json)
            self.solr_update(solr_json_text, req_id=response.meta['id'])
            # TODO commit one doc in one go with {add: {doc:}} json structure
            self.solr_update('{"commit": {}}', req_id=response.meta['id'])

        if response.meta['save_copies']:
            # Save the page.
            self.save_page_to_disk(FILEDB_PATH, response.url, response.text, response.meta)
