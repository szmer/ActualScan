import os, os.path
import http.client
import time, datetime
import urllib

import simpleflock

import scrapy
from sqlalchemy.ext.automap import automap_base
from sqlalchemy.orm import Session
from sqlalchemy import create_engine

from genscrap.flask_instance.settings import SQLALCHEMY_DATABASE_URI 

FILEDB_PATH = ''

#
# Page filing.
#
utc_offset_sec = time.altzone if time.localtime().tm_isdst else time.timezone
utc_offset = datetime.timedelta(seconds=-utc_offset_sec)
def now_timestamp():
    return datetime.datetime.now().replace(tzinfo=datetime.timezone(offset=utc_offset)).isoformat()

def write_to_file(path, content):
    with open(path, 'w+') as out_file:
        print(content, file=out_file)

class WebPage():
    def __init__(self, url, text, timestamp, tags, comment):
        self.url = url
        self.text = text
        self.timestamp = timestamp
        self.tags = tags
        self.comment = comment

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

    def start_requests(self):
        while True:
            requests_waiting = ScrapRequest.query.filter_by(status='waiting')
            for request in requests_waiting:
                request.status = 'ran'
                pg_session.commit()
                yield scrapy.Request(url=request.url, callback=self.parse,
                        meta={attr: getattr(request, attr)
                            for attr in ['id', 'is_search', 'source_type', 'save_copies']})
            else:
                time.sleep(0.2)

    def parse(self, response):
        if response.meta['is_search']:
            source_type = 'searchpage'
        else:
            source_type = response.meta['source_type']
        stractor_addr = 'speechtractor:3756/api/v01/interpret'
        params = urllib.parse.urlencode({'html': response.text, 'sourcetype': source_type})
        stractor_response = http.client.HTTPConnection(stractor_addr).request(
                'POST', '', params).getresponse()

        if stractor_response.status != 200:
            db_request = ScrapRequest.get(response.meta['id'])
            db_request.status = 'failed'
            db_request.failure_comment = 'Could not reach Speechtractor, status code {}'.format(
                    stractor_response.status) 
            pg_session.commit()
        else:
            solr_addr = 'http://solr:8983/solr/lookupy/update'
            solr_response = http.client.HTTPConnection(solr_addr).request(
                'GET', body=stractor_response.text).getresponse()
            if solr_response.status != 200:
                db_request = ScrapRequest.get(response.meta['id'])
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
