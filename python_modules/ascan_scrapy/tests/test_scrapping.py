import http.client
import json
import os
from time import sleep
import urllib

import pytest

from scan.models import Site, ScanJob, ScrapeRequest

SOLR_HOST = os.environ['SOLR_HOST']
SOLR_PORT = os.environ['SOLR_PORT']
SOLR_CORE = os.environ['SOLR_CORE']

def solr_search_json(query):
    """
    Query Solr directly, independently of the omnivore infrastructure.
    """
    query_str = '/solr/{}/select?q='.format(SOLR_CORE) + urllib.parse.quote(query, safe='\\')
    get_conn = http.client.HTTPConnection(SOLR_HOST, port=SOLR_PORT)
    get_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
    get_response = get_conn.getresponse()
    get_response_text = get_response.read().decode('utf-8')
    get_response_json = json.loads(get_response_text)
    return get_response_json

@pytest.mark.django_db
class TestScrapping():
    def test_scrapping_process(self, scrapy_process):
        # Clean up entries from the test domain in Solr.
        req_data = '{"delete": {"query": "site_name:quotes.toscrape.com"}, "commit": {}}'
        del_conn = http.client.HTTPConnection(SOLR_HOST, port=SOLR_PORT)
        del_conn.request('GET', '/solr/{}/update'.format(SOLR_CORE), body=req_data,
                headers={'Content-type': 'application/json'})
        del_response = del_conn.getresponse()
        assert del_response.status == 200
        # Check the deletion.
        solr_response_json = solr_search_json('site_name:quotes.toscrape.com')
        assert solr_response_json['response']['numFound'] == 0

        quotes_site = Site.objects.create(homepage_url='http://quotes.toscrape.com',
                search_pointer='http://quotes.toscrape.com/tag/twenty+cats/page/1',
                source_type='blog', site_name='quotes.toscrape.com',
                site_type='web')
        test_job = ScanJob.objects.create(user_ip='127.0.0.1', status='working',
                query_phrase='inspirational', website_count=1)
        search_request = ScrapeRequest.objects.create(target='http://quotes.toscrape.com/tag/',
                is_search=True, site=quotes_site, site_name=quotes_site.site_name,
                site_type=quotes_site.site_type,
                site_url=quotes_site.homepage_url, job=test_job, status='waiting')
        # Now the request should be picked up by the spider.
        second_request = False
        for try_n in range(20):
            sleep(0.25)
            try:
                second_request = ScrapeRequest.objects.get(is_search=False, status='committed')
                break
            except ScrapeRequest.DoesNotExist:
                pass
        assert second_request
        search_request = ScrapeRequest.objects.get(is_search=True)
        assert search_request.status == 'committed'
        solr_response_json = solr_search_json('site_name:quotes.toscrape.com')
        assert solr_response_json['response']['numFound'] > 0
