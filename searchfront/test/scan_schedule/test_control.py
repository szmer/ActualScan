from datetime import datetime, timezone
import http.client
import json
from time import sleep
import urllib.parse

from searchfront.blueprints.scan_schedule.models import ScanJob, ScrapRequest
from searchfront.blueprints.scan_schedule.control import (request_scan, start_scan,
        scan_progress_info)

class TestScanSchedule(object):
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
        existing_jobs = list(ScanJob.query.filter_by(id=job_id))
        if len(existing_jobs) != 0:
            db.session.delete(existing_jobs[0])
            db.session.commit()
        prescan_time = datetime.now(timezone.utc)
        request_scan('ex@example.com', 'inspirational', 'fun')

        # Ensure that the job is present, was just created and is inspectable.
        existing_jobs = list(ScanJob.query.filter_by(id=job_id))
        assert len(existing_jobs) == 1
        assert existing_jobs[0].last_checked >= prescan_time
        job = existing_jobs[0]
        progress_info = scan_progress_info(job)
        assert 'phase' in progress_info
        assert progress_info['phase'] == 'waiting'

        # We just manually start the requested scan.
        ScrapRequest.query.filter_by(job_id=job.id).delete() # note the constructed id persists
        start_scan(job)

        # Check the completion.
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
        requests = list(ScrapRequest.query.filter_by(job_id=job.id))
        assert len(requests) >= 3
        assert len([req for req in requests if req.status == 'ran']) >= 2
        assert len([req for req in requests if req.is_search == True]) == 1
        assert len([req for req in requests if req.source_type == 'blog']) >= 2

### TODO test handling 404 responses
