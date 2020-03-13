from datetime import datetime, timezone
import http.client
import json
from logging import info
import os, os.path
from urllib.parse import urlparse

import simpleflock

import scrapy
from sqlalchemy.ext.automap import automap_base
from sqlalchemy.orm import Session
from sqlalchemy import create_engine

from genscrap.flask_instance.settings import SQLALCHEMY_DATABASE_URI 
from genscrap.lib import (now_timestamp, write_to_file, WebPage,
        stractor_reading, timestamp_now,
        full_url)

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

ScrapRequest = AutomapBase.classes.scrap_request
pg_session = Session(pg_engine)

#
# The spider.
#

class GeneralSpider(scrapy.Spider):
    name = 'general'
    # We want to catch them to notify the DB of failure.
    handle_httpstatus_list = [400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413,
            414, 415, 416, 417, 500, 501, 502, 503, 504, 505]
    start_urls = [SOLR_PING_URL]

    def fail(self, failure):
        self.logger.error(repr(failure))

    def parse(self, response):
        # TODO be sure to date status changes
        # TODO decompose into different functions
        # Update the status of non-monitoring requests.
        if 'id' in response.meta:
            db_request = pg_session.query(ScrapRequest).get(response.meta['id'])
            db_request.status_changed = datetime.now(timezone.utc)
            # Notify of failures if needed.
            if response.status != 200:
                db_request.status = 'failed'
                db_request.failure_comment = 'Status code: {}'.format(response.status)
            else:
                db_request.status = 'ran'
            pg_session.commit()
            if response.status != 200:
                return

        # Solr dummy monitoring requests - we want to search Postgres for new requests.
        ####warning(urlparse(response.url))
        if urlparse(response.url).netloc == SOLR_NETLOC:
            scrap_requests_waiting = list(pg_session.query(ScrapRequest).filter_by(
                status='waiting'))
            requests_done = False
            for scrap_request in scrap_requests_waiting:
                scrap_request.status = 'scheduled'
                meta = {attr: getattr(scrap_request, attr)
                            for attr
                            in REQUEST_META}
                pg_session.commit()
                url = full_url(scrap_request.url, scrap_request.site_url)
                yield scrapy.Request(url=url, callback=self.parse, meta=meta,
                        errback=self.fail)
                requests_done = True
            if not requests_done:
                yield scrapy.Request(url=SOLR_PING_URL, callback=self.parse, errback=self.fail,
                        # Avoid the request getting caught in deduplication.
                        dont_filter=True)
            return
        # Check if a new monitoring request is necessary.
        else:
            scrap_requests_waiting_count = pg_session.query(ScrapRequest).filter_by(
                status='waiting').count()
            if scrap_requests_waiting_count == 0:
                yield scrapy.Request(url=SOLR_PING_URL, callback=self.parse, errback=self.fail,
                        # Avoid the request getting caught in deduplication.
                        dont_filter=True)

        # Normal scraping requests.
        if response.meta['is_search']:
           source_type = 'searchpage'
        else:
            source_type = response.meta['source_type']
        stractor_output = stractor_reading(response.text, source_type)
        # Speechtractor failures.
        if isinstance(stractor_output, int):
            db_request = pg_session.query(ScrapRequest).get(response.meta['id'])
            db_request.status = 'failed'
            db_request.failure_comment = 'Could not reach Speechtractor, status code {}'.format(
                    stractor_output) 
            pg_session.commit()
        # Handling search pages.
        elif response.meta['is_search']:
            # TODO go to the next page!
            # may cause JSONDecodeError
            stractor_response_json = json.loads(stractor_output)
            for found_page in stractor_response_json:
                if 'url' in found_page:
                    scrap_request = ScrapRequest(url=found_page['url'], is_search=False,
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
                                in REQUEST_META}, dont_filter=True)
        # The actual page indexing.
        else:
            # may cause JSONDecodeError
            stractor_response_json = json.loads(stractor_output)
            # TODO correct permalink and such!!!
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
            solr_conn = http.client.HTTPConnection('solr', port=8983, timeout=10)
            info('Sent to Solr: {}'.format(solr_json_text))
            solr_conn.request('GET', '/solr/lookupy/update', body=solr_json_text,
                headers={'Content-type': 'application/json'})
            solr_response = solr_conn.getresponse()
            info('Solr response: {}'.format(solr_response.status))
            if solr_response.status != 200:
                db_request = pg_session.query(ScrapRequest).get(response.meta['id'])
                db_request.status = 'failed'
                db_request.failure_comment = 'Could not reach Solr, status code {}'.format(
                        solr_response.status) 
                pg_session.commit()
            # TODO commit one doc in one go with {add: {doc:}} json structure
            req_data = '{"commit": {}}'
            solr_conn.request('GET', '/solr/lookupy/update', body=req_data,
                headers={'Content-type': 'application/json'})
            info('Solr commit response: {}'.format(solr_response.status))
            if solr_response.status != 200:
                db_request = pg_session.query(ScrapRequest).get(response.meta['id'])
                db_request.status = 'failed'
                db_request.failure_comment = 'Could not reach Solr for commit, status code {}'.format(
                        solr_response.status) 
                pg_session.commit()

        if response.meta['save_copies']:
            # Save the page.
            page_obj = WebPage(response.url, response.text, now_timestamp(), self.tags,
                    self.comment)
            self.file_page(page_obj, FILEDB_PATH)

    def file_page(self, page_obj, db_path):
        with simpleflock.SimpleFlock(db_path+'index.csv.lock'):
            if os.path.isfile(db_path+'index.csv'):
                with open(db_path+'index.csv', 'r') as db_index_file:
                    entry_n = len(db_index_file.readlines())
            else:
                entry_n = 0
            db_catalog = db_path + str(entry_n) + '/'
            os.makedirs(db_catalog, exist_ok=False)
            write_to_file(db_catalog+'index', page_obj.text)
            write_to_file(db_catalog+'url', page_obj.url)
            write_to_file(db_catalog+'access-timestamp', page_obj.timestamp)
            write_to_file(db_catalog+'tags', ' '.join(page_obj.tags))
            write_to_file(db_catalog+'comment', page_obj.comment)
            # Append the db entry for this page.
            with open(db_path+'index.csv', 'a+') as db_index_file:
                print(entry_n, page_obj.url, file=db_index_file)
