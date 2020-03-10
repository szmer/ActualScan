from datetime import datetime, timezone
import http.client
import json
from logging import warning
import os, os.path
import time

import simpleflock

import scrapy
from sqlalchemy.ext.automap import automap_base
from sqlalchemy.orm import Session
from sqlalchemy import create_engine

from genscrap.flask_instance.settings import SQLALCHEMY_DATABASE_URI 
from genscrap.lib import now_timestamp, write_to_file, WebPage, stractor_reading

FILEDB_PATH = ''

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

    def start_requests(self):
        # NOTE NOTE allowing an endless loop here means that scrapy never continues to do subsequent
        # requests
        #while True:
        scrap_requests_waiting = list(pg_session.query(ScrapRequest).filter_by(status='waiting'))
        for scrap_request in scrap_requests_waiting:
            scrap_request.status = 'ran'
            meta = {attr: getattr(scrap_request, attr)
                        for attr
                        in ['id', 'job_id', 'is_search', 'source_type', 'save_copies']}
            #scrap_request.failure_comment = str(meta)
            pg_session.commit()
            yield scrapy.Request(url=scrap_request.url, callback=self.parse, meta=meta,
                    errback=self.fail)
           # else:
        #        time.sleep(0.2)

    def fail(self, failure):
        self.logger.error(repr(failure))
        scrap_requests_ran = pg_session.query(ScrapRequest).filter_by(status='ran')
        for scrap_request in scrap_requests_ran:
            scrap_request.status = 'failed'
            pg_session.commit()

    def parse(self, response):
        if response.status != 200:
            db_request = pg_session.query(ScrapRequest).get(response.meta['id'])
            db_request.status = 'failed'
            db_request.failure_comment = 'Status code: {}'.format(response.status)
            pg_session.commit()
            return

        if response.meta['is_search']:
            source_type = 'searchpage'
        else:
            source_type = response.meta['source_type']

        stractor_output = stractor_reading(response.text, source_type)
        warning(stractor_output)
        if isinstance(stractor_output, int):
            db_request = pg_session.query(ScrapRequest).get(response.meta['id'])
            db_request.status = 'failed'
            db_request.failure_comment = 'Could not reach Speechtractor, status code {}'.format(
                    stractor_output) 
            pg_session.commit()
        elif response.meta['is_search']:
            # may cause JSONDecodeError
            stractor_response_json = json.loads(stractor_output)
            for found_page in stractor_response_json:
                if 'url' in found_page:
                    scrap_request = ScrapRequest(url=found_page['url'], is_search=False,
                            job_id=response.meta['job_id'], status='waiting',
                            status_changed=datetime.now(timezone.utc),
                            save_copies=response.meta['save_copies'])
                    pg_session.add(scrap_request)
                    pg_session.commit()
                    yield response.follow(found_page['url'],
                            callback=self.parse,
                            meta={attr: getattr(scrap_request, attr) for attr
                                in ['id', 'job_id', 'is_search', 'source_type', 'save_copies']})
        else:
            # TODO correct permalink and such!!!
            solr_conn = http.client.HTTPConnection('solr', port=8983, timeout=10)
            solr_conn.request('GET', '/solr/lookupy/update', body=stractor_output)
            solr_response = solr_conn.getresponse()
            if solr_response.status != 200:
                db_request = pg_session.query(ScrapRequest).get(response.meta['id'])
                db_request.status = 'failed'
                db_request.failure_comment = 'Could not reach Solr, status code {}'.format(
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
