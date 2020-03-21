from datetime import datetime, timezone
import http.client
import json
from time import sleep
import urllib.parse

from searchfront.blueprints.scan_schedule.models import ScanJob, ScrapeRequest
from searchfront.blueprints.scan_schedule.control import (request_scan, terminate_scan,
        scan_progress_info, do_scan_management)

class TestScanSchedule(object):
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

        # Check if Scrapy and Speechtractor are up.
        assert scrapyp.process.poll() is None
        stractor_conn = http.client.HTTPConnection('speechtractor', port=3756)
        stractor_conn.request('GET', '/api/v01/status')
        stractor_response = stractor_conn.getresponse()
        assert stractor_response.status == 200
        assert stractor_response.read() == b'ok'

        # Create the scan job.
        job_id = ScanJob.identifier('ex@example.com', 'inspirational', ['fun'])
        # Remove the previous one if exists.
        ###-ScrapeRequest.query.filter_by(job_id=job_id).delete() # note the constructed id persists
        existing_job = ScanJob.query.get(job_id)
        if existing_job:
            terminate_scan(existing_job)
        prescan_time = datetime.now(timezone.utc)
        request_scan('ex@example.com', 'inspirational', 'fun')

        # Ensure that the job is present, was just created and is inspectable.
        job = ScanJob.query.get(job_id)
        assert job.last_checked >= prescan_time
        progress_info = scan_progress_info(job)
        assert 'phase' in progress_info
        assert progress_info['phase'] == 'waiting'

        # This should start the scan.
        scheduling_info = do_scan_management()
        assert 'spare_capacity' in scheduling_info

        # Check the completion.
        progress_info = scan_progress_info(job)
        assert 'phase' in progress_info
        assert progress_info['phase'] == 'working'
        query_str = '/solr/lookupy/select?q=' + urllib.parse.quote(
                'site_name:quotes.toscrape.com')
        # NOTE we currently need a long time due to toscrap.com redirections.
        for i in range(120*2): # wait up to 120 sec
            sleep(0.5)
            ####-assert scrapyp.process.poll() is None
            get_conn = http.client.HTTPConnection('solr', port=8983)
            get_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
            get_response = get_conn.getresponse()
            get_response_text = get_response.read().decode('utf-8')
            #print(get_response_text)
            get_response_json = json.loads(get_response_text)
            if ('response' in get_response_json and 'docs' in get_response_json['response']
                and len(get_response_json['response']['docs']) > 0):
                break
        assert 'response' in get_response_json and 'docs' in get_response_json['response']
        assert len(get_response_json['response']['docs']) > 0

        # Find the done requests in the DB.
        requests = list(ScrapeRequest.query.filter_by(job_id=job.id))
        assert len(requests) >= 3
        assert len([req for req in requests if req.status == 'ran']) >= 2
        assert len([req for req in requests if req.is_search == True]) == 1
        assert len([req for req in requests if req.source_type == 'blog']) >= 2

### TODO test handling 404 responses
