import http.client
import json
from time import sleep

from searchfront.scrapy_process import scrapyp

from searchfront.blueprints.scan_schedule.models import ScanJob, ScrapRequest
from searchfront.blueprints.scan_schedule.control import (request_scan, start_scan,
        scan_progress_info)

class TestScanSchedule(object):
    def test_perform_scan(self):
        # Clean up entries from the test domain in Solr.
        req_data = '<delete><query>site:szymonrutkowski.pl*</query></delete>'
        del_response = http.client.HTTPConnection('solr:8983/solr/lookupy/update').request(
                'GET', body=req_data).getresponse()
        assert del_response.status == 200

        # Check if Scrapy and Speechtractor are up.
        assert scrapyp.poll() is None
        stractor_response = http.client.HTTPConnection('speechtractor:3756/api/v01/status').request(
                'GET').getresponse()
        assert stractor_response.status == 200
        assert stractor_response.text == 'ok'

        # Create the scan job.
        job_id = ScanJob.identifier('ex@example.com', 'Mroziński', ['fun'])
        request_scan('ex@example.com', 'Mroziński', ['fun'])

        # Ensure that the job was create and is inspectable.
        jobs = list(ScanJob.query.filter_by(id=job_id))
        assert len(jobs) == 1
        job = jobs[0]
        progress_info = scan_progress_info(job)
        assert 'phase' in progress_info
        assert progress_info['phase'] == 'waiting'

        # We just manually start the requested scan.
        start_scan(job)

        # Check the completion.
        query_str = ('http://solr:8983/solr/lookupy/select?q=text:Mroziński'
                +' AND site:szymonrutkowski.pl*')
        for i in range(10*5): # wait up to 10 sec
            sleep(0.2)
            get_response = http.client.HTTPConnection(query_str).request('GET').getresponse()
            get_response_json = json.loads(get_response)
            if ('response' in get_response_json and 'docs' in get_response_json['response']
                and len(get_response_json['response']['docs']) > 0):
                break
        assert ('response' in get_response_json and 'docs' in get_response_json['response']
                and len(get_response_json['response']['docs']) > 0)

        # Find the done requests in the DB.
        requests = list(ScrapRequest.query.filter_by(job_id=job.id))
        assert len(requests) >= 3
        assert len([req for req in requests if req.status == 'ran']) >= 2
        assert len([req for req in requests if req.is_search == True]) == 1
        assert len([req for req in requests if req.source_type == 'blog']) >= 2
