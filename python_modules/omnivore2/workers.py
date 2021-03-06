from base64 import b64encode
from datetime import timedelta
import http.client
import json
from logging import info
import os
import random
from threading import Thread
from time import sleep
from urllib import parse
import ssl

from redis import Redis

from omnivore2_conf import (
        MINIMUM_CONTEXT_SIZE, MAXIMUM_CONTEXT_SIZE, SOLR_HOST, SOLR_PORT, SOLR_CORE,
        SOLR_UPDATER_PASS, RECLASSIFICATION_TIME
        )
from contextual_analysis import apply_contextual_analysis
from utils import date_fmt, time_now

redis = Redis(host='redis', port=6379, db=0, password=os.environ['REDIS_PASS'], ssl=True,
        ssl_certfile='/home/certs/ascan_internal.pem',
        ssl_keyfile='/home/certs/ascan_internal_key.key',
        ssl_ca_certs='/home/certs/ascan_internal.pem')
WORKERS_COUNT = int(os.environ.get('OMNIVORE2_WORKERS_COUNT', 1))

ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
ssl_context.load_verify_locations('/home/certs/ascan_internal.pem')

class ContextualAnalyzerThread(Thread):
    def run(self):
        while True:
            sleep(5)
            query_string = redis.blpop(['queue:index_query'], 10)
            if not query_string:
                # First, get the top tags.
                solr_address = '/solr/{}/terms?terms.fl=tags&terms.limit=1000'.format(SOLR_CORE)
                headers = { 'Authorization':
                        'Basic {}'.format(
                            b64encode(bytes('updater:'+SOLR_UPDATER_PASS, 'utf-8')).decode('ascii'))
                        }
                solr_conn = http.client.HTTPSConnection(SOLR_HOST,
                        port=SOLR_PORT, timeout=15, context=ssl_context)
                try:
                    solr_conn.request('GET', solr_address, headers=headers)
                    response = solr_conn.getresponse()
                    response_json = json.loads(response.read().decode('utf-8'))
                    top_tags = [response_json['terms']['tags'][n]
                            for n in range(len(response_json['terms']['tags']))
                            if n % 2 == 0] # the even entries are frequencies
                except Exception as e:
                    info('Encountered exception when connecting to Solr for tags: {}'.format(e))
                    continue
                if not top_tags: # an empty list
                    continue
                tag_to_use = random.choice(top_tags)
                solr_address = '/solr/{}/select?defType=dismax&q={}&qf=tags'.format(
                        SOLR_CORE, tag_to_use)
            else:
                # Reuse the last Solr query. Get the rows with no classification date or an old date.
                solr_address = query_string[1].decode('utf-8')
            # Add the necessary filtering and rows limit for getting the context to use.
            solr_address += '&fq=' + parse.quote(
                    # apparently that's how you detect empty fields in AND/OR constructions
                    'date_class: [{} TO *] OR (*:* AND -date_class:[* TO *])'.format(
                        date_fmt(time_now()
                            - timedelta(hours=RECLASSIFICATION_TIME))),
                        safe='')
            solr_address += '&rows={}'.format(MAXIMUM_CONTEXT_SIZE)
            headers = { 'Authorization':
                    'Basic {}'.format(
                        b64encode(bytes('updater:'+SOLR_UPDATER_PASS, 'utf-8')).decode('ascii')) }
            solr_conn = http.client.HTTPSConnection(SOLR_HOST,
                    port=SOLR_PORT, timeout=15, context=ssl_context)
            try:
                solr_conn.request('GET', solr_address, headers=headers)
                response = solr_conn.getresponse()
                response_json = json.loads(response.read().decode('utf-8'))
                docs = response_json['response']['docs']
            except Exception as e:
                info('Encountered exception when connecting to Solr: {}'.format(e))
                continue
            if len(docs) < MINIMUM_CONTEXT_SIZE:
                continue
            apply_contextual_analysis(docs)
            solr_conn.request('GET', '/solr/{}/update?commit=true'.format(SOLR_CORE),
                    body=json.dumps(docs),
                    headers={'Content-type': 'application/json'})

workers = []
def start_workers():
    for worker_n in range(WORKERS_COUNT):
        thread = ContextualAnalyzerThread()
        workers.append(thread)
        thread.start()
