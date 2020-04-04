from datetime import datetime, timezone
import http.client
import json
from time import sleep
import urllib.parse

import pytest

from searchfront.blueprints.scan_schedule.models import ScanJob, ScrapeRequest
from searchfront.blueprints.scan_schedule.control import (request_scan, terminate_scan,
        scan_progress_info, do_scan_management)

def solr_search_json(query):
    query_str = '/solr/lookupy/select?q=' + urllib.parse.quote(query, safe='\\')
    get_conn = http.client.HTTPConnection('solr', port=8983)
    get_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
    get_response = get_conn.getresponse()
    get_response_text = get_response.read().decode('utf-8')
    get_response_json = json.loads(get_response_text)
    return get_response_json

@pytest.mark.with_network
class TestScanSchedule(object):
    def test_scan_environment(self, db, scrapyp, redditp):
        # Check if Scrapy and Speechtractor are up.
        assert scrapyp.process.poll() is None # a call to the Subprocess object
        assert redditp.process.poll() is None # a call to the Subprocess object
        stractor_conn = http.client.HTTPConnection('speechtractor', port=3756)
        stractor_conn.request('GET', '/api/v01/status')
        stractor_response = stractor_conn.getresponse()
        assert stractor_response.status == 200
        assert stractor_response.read() == b'ok'

        # Create the scan job.
        # Note that all test site types are "fun".
        job_id = ScanJob.identifier('ex@example.com', 'NabuchodonozorKopieJeftego?', ['fun'])
        existing_job = ScanJob.query.get(job_id)
        if existing_job:
            terminate_scan(existing_job)
        prescan_time = datetime.now(timezone.utc)
        request_scan('ex@example.com', 'NabuchodonozorKopieJeftego?', ['fun'])

        # Ensure that the job is present, was just created and is inspectable.
        job = ScanJob.query.get(job_id)
        assert job.last_checked >= prescan_time
        progress_info = scan_progress_info(job)
        assert 'phase' in progress_info
        assert progress_info['phase'] == 'waiting'

        # This should start the scan.
        scheduling_info = do_scan_management()
        assert 'spare_capacity' in scheduling_info

        # Test the status.
        progress_info = scan_progress_info(job)
        assert 'phase' in progress_info
        assert progress_info['phase'] in ['search', 'crawl', 'done']
        reqs = list(ScrapeRequest.query.filter_by(job_id=job.id))
        # (avoid request type cross-contamination)
        assert 'quotes.toscrape.com' in [req.site_name for req in reqs]
        assert '/r/test' in [req.site_name for req in reqs]

        # Terminate it immediately.
        terminate_scan(job)
        assert job.status == 'terminated'
        job_reqs = list(job.requests)
        assert len(job_reqs) > 0
        assert set([req.status for req in job_reqs]) <= set(['ran', 'failed', 'cancelled'])
        progress_info = scan_progress_info(job)
        assert progress_info['phase'] in ['terminated', 'finished']

    # NOTE NOTE This test assumes that we are using a test database. Particularly no legitimate
    # scan jobs are present and no stray do_scan_management calls will be made.
    def test_perform_scan(self, app, db, scrapyp):
        # Clean up entries from the test domain in Solr.
        req_data = '{"delete": {"query": "site_name:quotes.toscrape.com"}, "commit": {}}'
        del_conn = http.client.HTTPConnection('solr', port=8983)
        del_conn.request('GET', '/solr/lookupy/update', body=req_data,
                headers={'Content-type': 'application/json'})
        del_response = del_conn.getresponse()
        assert del_response.status == 200
        # Check the deletion.
        solr_response_json = solr_search_json('site_name:quotes.toscrape.com')
        assert 'response' in solr_response_json and 'numFound' in solr_response_json['response']
        assert solr_response_json['response']['numFound'] == 0

        # Create the scan job.
        job_id = ScanJob.identifier('ex@example.com', 'inspirational', ['games'])
        existing_job = ScanJob.query.get(job_id)
        if existing_job:
            terminate_scan(existing_job)
        request_scan('ex@example.com', 'inspirational', ['games'])

        # This should start the scan.
        do_scan_management()

        # Check the completion.
        job = ScanJob.query.get(job_id)
        # NOTE we currently need a long time due to toscrap.com redirections.
        for i in range(120*2): # wait up to 120 sec
            sleep(0.5)
            #assert scrapyp.process.poll() is None
            solr_response_json = solr_search_json('site_name:quotes.toscrape.com')
            if ('response' in solr_response_json and 'docs' in solr_response_json['response']
                and len(solr_response_json['response']['docs']) > 0):
                break
        assert 'response' in solr_response_json and 'docs' in solr_response_json['response']
        assert len(solr_response_json['response']['docs']) > 0
        # TODO ensure that we have site tags, and not query tags in the index!

        # Find the done requests in the DB.
        requests = list(ScrapeRequest.query.filter_by(job_id=job.id))
        assert len(requests) >= 2
        assert len([req for req in requests if req.status == 'ran']) >= 2
        assert len([req for req in requests if req.is_search == True]) == 1
        assert len([req for req in requests if req.source_type == 'blog']) >= 2

    def test_reddit_scan(self, app, db, redditp):
        # Clean up entries from the test domain in Solr (note that slashes must be escaped in the
        # query)
        req_data = ('{"delete": {"query": "site_name:\\"'
                # NOTE This need not be url-encoded, but we have to double-escape backslashes to
                # make them reach Solr and actually escape the slashes. Otherwise we get something
                # like "a _text_ field doesn't exist" error.
                +'\\\\/r\\\\/test'
                +'\\""}, "commit": {}}')
        del_conn = http.client.HTTPConnection('solr', port=8983)
        del_conn.request('GET', '/solr/lookupy/update', body=req_data,
                headers={'Content-type': 'application/json'})
        del_response = del_conn.getresponse()
        assert del_response.status == 200
        # Check the deletion.
        solr_response_json = solr_search_json('site_name:\\/r\\/test')
        assert 'response' in solr_response_json and 'numFound' in solr_response_json['response']
        assert solr_response_json['response']['numFound'] == 0

        # Create the scan job.
        # ("jour" seems to have at least one submission with comments)
        job_id = ScanJob.identifier('ex@example.com', 'jour', ['reddit'])
        existing_job = ScanJob.query.get(job_id)
        if existing_job:
            terminate_scan(existing_job)
        request_scan('ex@example.com', 'jour', ['reddit'])

        # This should start the scan.
        do_scan_management()

        # Check the completion.
        job = ScanJob.query.get(job_id)
        for i in range(120*2): # wait up to 120 sec
            sleep(0.5)
            #assert redditp.process.poll() is None
            solr_response_json = solr_search_json('site_name:\\/r\\/test')
            if ('response' in solr_response_json and 'docs' in solr_response_json['response']
                and len(solr_response_json['response']['docs']) > 0):
                break
        assert 'response' in solr_response_json and 'docs' in solr_response_json['response']
        assert len(solr_response_json['response']['docs']) > 0

        # Find the done requests in the DB.
        requests = list(ScrapeRequest.query.filter_by(job_id=job.id))
        assert len(requests) >= 2
        assert len([req for req in requests if req.status == 'ran']) >= 2
        assert len([req for req in requests if req.is_search == True]) == 1
        assert len([req for req in requests if req.source_type == 'forums']) >= 2

### TODO test handling 404 responses
