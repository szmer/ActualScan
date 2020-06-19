from datetime import datetime, timezone
import http.client
import json
#import logging
from time import sleep
import urllib.parse

import pytest
from sqlalchemy.orm.session import Session

from searchfront.blueprints.scan_schedule.models import ScanJob, ScrapeRequest
from searchfront.blueprints.scan_schedule.control import (request_scan, terminate_scan,
        scan_progress_info)

def solr_search_json(query):
    query_str = '/solr/lookupy/select?q=' + urllib.parse.quote(query, safe='\\')
    get_conn = http.client.HTTPConnection('solr', port=8983)
    get_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
    get_response = get_conn.getresponse()
    get_response_text = get_response.read().decode('utf-8')
    get_response_json = json.loads(get_response_text)
    return get_response_json

@pytest.mark.with_network
class TestScanScheduleControl(object):
    def test_scan_environment(self, db, scrapyp, redditp, example_user):
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
        existing_jobs = list(ScanJob.query.filter_by(query_phrase='NabuchodonozorKopieJeftego?',
                query_tags='fun', user_id=example_user.id))
        if existing_jobs:
            terminate_scan(existing_jobs[0].id)
        prescan_time = datetime.now(timezone.utc)
        job = request_scan(example_user.id, 'NabuchodonozorKopieJeftego?', ['fun'], force_new=True)

        # Ensure that the job is present, was just created and is inspectable.
        assert job.last_checked >= prescan_time

        # NOTE It's important to invalidate the object, get the updated one next time.
        job_id = job.id
        session = Session.object_session(job)
        session.commit()

        # This should start the scan, given that the Celery task works.
        sleep(2.0)

        # Test the status.
        progress_info = scan_progress_info(job.id)
        assert 'phase' in progress_info
        assert progress_info['phase'] in ['search', 'crawl', 'done']
        reqs = list(ScrapeRequest.query.filter_by(job_id=job_id))
        # (avoid request type cross-contamination)
        assert 'quotes.toscrape.com' in [req.site_name for req in reqs]
        assert '/r/test' in [req.site_name for req in reqs]

        # Terminate it immediately.
        terminate_scan(job_id)
        job = ScanJob.query.get(job_id)
        assert job.status == 'terminated'
        job_reqs = list(job.requests)
        assert len(job_reqs) > 0
        assert set([req.status for req in job_reqs]) <= set(['ran', 'committed', 'failed',
            'cancelled'])
        progress_info = scan_progress_info(job_id)
        assert progress_info['phase'] in ['terminated', 'finished']

    # NOTE NOTE This test assumes that we are using a test database. Particularly no conflicting
    # legitimate scan jobs are present.
    def test_perform_scan(self, app, db, scrapyp, example_user):
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
        existing_jobs = list(ScanJob.query.filter_by(query_phrase='inspirational', query_tags='games',
                user_id=example_user.id))
        if existing_jobs:
            terminate_scan(existing_jobs[0].id)
        job = request_scan(example_user.id, 'inspirational', ['games'], force_new=True)

        # This should start the scan, given that the Celery task works.
        sleep(1.5)

        # Check the completion.
        proportions = []
        # NOTE we currently need a long time due to toscrap.com redirections.
        for i in range(120*2): # wait up to 120 sec
            sleep(0.5)
            progress_info = scan_progress_info(job.id)
            proportions.append(progress_info['dl_proportion'])
            # We check requests status instead of Solr directly to avoid racing with updating those
            # statuses.
            requests_committed = list(ScrapeRequest.query.filter_by(job_id=job.id,
                status='committed'))
            if len(requests_committed) >= 6: # two searches (prioritized), four crawls
                break
        solr_response_json = solr_search_json('site_name:quotes.toscrape.com')
        assert 'response' in solr_response_json and 'docs' in solr_response_json['response']
        assert len(solr_response_json['response']['docs']) > 0
        # TODO ensure that we have site tags, and not query tags in the index!

        # We should also have the request for the second search page.
        assert len([req for req in requests_committed if req.is_search == True]) == 2
        assert len([req for req in requests_committed if req.source_type == 'blog']) >= 2

        # The reported proportion should never decrease.
        assert all(x<=y for x, y in zip(proportions, proportions[1:]))

        ### TODO test handling 404 responses

    def test_reddit_scan(self, app, db, redditp, example_user):
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
        existing_jobs = list(ScanJob.query.filter_by(query_phrase='jour', query_tags='reddit',
                user_id=example_user.id))
        if existing_jobs:
            terminate_scan(existing_jobs[0].id)
        job = request_scan(example_user.id, 'jour', ['reddit'], force_new=True)

        # This should start the scan, given that the Celery task works.
        sleep(1.5)

        # Check the completion.
        proportions = []
        for i in range(120*2): # wait up to 120 sec
            sleep(0.5)
            progress_info = scan_progress_info(job.id)
            proportions.append(progress_info['dl_proportion'])
            # We check requests status instead of Solr directly to avoid racing with updating those
            # statuses.
            requests_committed = list(ScrapeRequest.query.filter_by(job_id=job.id,
                status='committed'))
            if len(requests_committed) >= 5:
                break
        solr_response_json = solr_search_json('site_name:\\/r\\/test')
        assert 'response' in solr_response_json and 'docs' in solr_response_json['response']
        assert len(solr_response_json['response']['docs']) > 0

        # Specifically, we cannot expect the search request to already commit (there may be more
        # submissions and it commits only after all sites for Reddit).
        requests = list(ScrapeRequest.query.filter_by(job_id=job.id))
        assert len([req for req in requests if req.is_search == True]) == 1
        assert len([req for req in requests if req.source_type != 'forums']) == 0

        # The reported proportion should never decrease.
        assert all(x<=y for x, y in zip(proportions, proportions[1:]))
