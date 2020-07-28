from datetime import datetime, timezone
import http.client
import json
from time import sleep
import urllib.parse

from django.test import Client, TestCase
from django.contrib.auth.models import User
from dynamic_preferences.registries import global_preferences_registry

from scan.models import Tag, Site, ScanJob, ScrapeRequest
from scan.control import (request_scan, terminate_scan,
        scan_progress_info)

def solr_search_json(query):
    """
    Query Solr directly, independently of the omnivore infrastructure.
    """
    query_str = '/solr/lookupy/select?q=' + urllib.parse.quote(query, safe='\\')
    get_conn = http.client.HTTPConnection('solr', port=8983)
    get_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
    get_response = get_conn.getresponse()
    get_response_text = get_response.read().decode('utf-8')
    get_response_json = json.loads(get_response_text)
    return get_response_json

class ScanProcessTestCase(TestCase):

    def setUp(self):
        # Setup example tags and sites.
        self.fun_tag = Tag.objects.create(name='fun', level=100000,
                description='Sites containing fun things.')
        self.games_tag = Tag.objects.create(name='games', level=100000, description='Games of all kinds.')
        self.reddit_tag = Tag.objects.create(name='reddit', level=100000,
                description='Various subreddits.')
        quotes_site = Site.objects.create(homepage_url='http://quotes.toscrape.com',
                level=100000,
                search_pointer='http://quotes.toscrape.com/tag/|||fat|||+|||cat|||/page/1',
                source_type='blog', site_name='quotes.toscrape.com',
                site_type='web')
        reddit_site = Site.objects.create(homepage_url='https://reddit.com/r/test',
                level=100000,
                search_pointer='test',
                source_type='forums', site_name='/r/test',
                site_type='reddit')
        quotes_site.tags.add(self.fun_tag)
        quotes_site.tags.add(self.games_tag)
        reddit_site.tags.add(self.fun_tag)
        reddit_site.tags.add(self.reddit_tag)

        # Add the example user.
        self.example_user = User.objects.create_user(username='example_username',
                email='john@example.com')
        self.example_user.set_password('example_pass')

        # Prepare the global preferences registry.
        self.global_preferences = global_preferences_registry.manager()

    def solr_purge_site(self, site_name):
        """
        Purge all documents from the site in Solr.
        """
        # Clean up entries from the test domain in Solr.
        # (the curly brackets need to be doubled)
        req_data = '{{"delete": {{"query": "site_name:{}"}}, "commit": {{}}}}'.format(site_name)
        del_conn = http.client.HTTPConnection('solr', port=8983)
        del_conn.request('GET', '/solr/lookupy/update', body=req_data,
                headers={'Content-type': 'application/json'})
        del_response = del_conn.getresponse()
        self.assertEqual(del_response.status, 200)
        # Check the deletion.
        solr_response_json = solr_search_json('site_name:quotes.toscrape.com')
        self.assertIn('response', solr_response_json)
        self.assertIn('numFound', solr_response_json['response'])
        self.assertEqual(solr_response_json['response']['numFound'], 0)

    def test_scan_environment(self):
        # Test that we have Scrapy, the Reddit scraper and Speechtractor available.
        #assert scrapyp.process.poll() is None # a call to the Subprocess object
        #assert redditp.process.poll() is None # a call to the Subprocess object
        stractor_conn = http.client.HTTPConnection('speechtractor', port=3756)
        stractor_conn.request('GET', '/api/v01/status')
        stractor_response = stractor_conn.getresponse()
        self.assertEqual(stractor_response.status, 200)
        self.assertEqual(stractor_response.read(), b'ok')

        prescan_time = datetime.now(timezone.utc)
        job = request_scan(self.example_user.id, 'NabuchodonozorKopieJeftego?', ['fun'], force_new=True)

        self.assertGreaterEqual(job.last_checked, prescan_time)

        # this should start the scan, given that the celery task works.
        sleep(2.0)

        # Test the status.
        progress_info = scan_progress_info(job.id)
        self.assertIn('phase', progress_info)
        self.assertIn(progress_info['phase'], ['search', 'crawl', 'done'])
        reqs = ScrapeRequest.objects.filter(job_id=job.id)
        # (avoid request type cross-contamination)
        self.assertIn('quotes.toscrape.com', [req.site_name for req in reqs])
        self.assertIn('/r/test', [req.site_name for req in reqs])

        # Terminate it immediately.
        terminate_scan(job.id)
        job = ScanJob.objects.get(job.id)
        self.assertEqual(job.status, 'terminated')
        self.assertGreater(len(job.requests), 0)
        self.assertLessEqual(set([req.status for req in job.requests]),
                set(['ran', 'committed', 'failed', 'cancelled']))
        progress_info = scan_progress_info(job.id)
        self.assertIn(progress_info['phase'], ['terminated', 'finished'])

    def test_web_scan(self):
        self.solr_purge_site('quotes.toscrape.com')

        job = request_scan(self.example_user.id, 'inspirational', ['games'], force_new=True)

        # This should start the scan, given that the Celery task works.
        sleep(1.5)

        # Check the completion.
        proportions = []
        # NOTE we currently need a long time due to toscrap.com redirections.
        for i in range(1*2): # wait up to 120 sec
            sleep(0.5)
            progress_info = scan_progress_info(job.id)
            if 'dl_proportion' in progress_info:
                proportions.append(progress_info['dl_proportion'])
            # We check requests status instead of Solr directly to avoid racing with updating those
            # statuses.
            requests_committed = ScrapeRequest.objects.filter(job_id=job.id,
                status='committed')
            if len(requests_committed) >= 6: # two search pages (prioritized), four crawls
                break
        solr_response_json = solr_search_json('site_name:quotes.toscrape.com')
        self.assertIn('response', solr_response_json)
        self.assertIn('docs', solr_response_json['response'])
        self.assertGreater(len(solr_response_json['response']['docs']), 0)
        # TODO ensure that we have site tags, and not query tags in the index!

        # We should also have the request for the second search page.
        self.assertEqual(len([req for req in requests_committed if req.is_search == True]), 2)
        self.assertGreaterEqual(len([req for req
            in requests_committed if req.source_type == 'blog']),
            2)

        # The reported proportion should never decrease.
        self.assertTrue(all(x<=y for x, y in zip(proportions, proportions[1:])))
        ### TODO test handling 404 responses

    def test_reddit_scan(self):
        self.solr_purge_site('\\"'
                # NOTE This need not be url-encoded, but we have to double-escape backslashes to
                # make them reach Solr and actually escape the slashes. Otherwise we get something
                # like "a _text_ field doesn't exist" error.
                +'\\\\/r\\\\/test'
                +'\\"')

        job = request_scan(self.example_user.id, 'jour', ['reddit'], force_new=True)

        # This should start the scan, given that the Celery task works.
        sleep(1.5)

        # Check the completion.
        proportions = []
        # NOTE we currently need a long time due to toscrap.com redirections.
        for i in range(1*2): # wait up to 120 sec
            sleep(0.5)
            progress_info = scan_progress_info(job.id)
            if 'dl_proportion' in progress_info:
                proportions.append(progress_info['dl_proportion'])
            # We check requests status instead of Solr directly to avoid racing with updating those
            # statuses.
            requests_committed = ScrapeRequest.objects.filter(job_id=job.id,
                status='committed')
            if len(requests_committed) >= 5:
                break
        solr_response_json = solr_search_json('site_name:\\r/\\/test')
        self.assertIn('response', solr_response_json)
        self.assertIn('docs', solr_response_json['response'])
        self.assertGreater(len(solr_response_json['response']['docs']), 0)
        # TODO ensure that we have site tags, and not query tags in the index!

        # Specifically, we cannot expect the search request to already commit (there may be more
        # submissions and it commits only after all sites in the Reddit scraper).
        requests = ScrapeRequest.query.filter_by(job_id=job.id)
        self.assertEqual(len([req for req in requests if req.is_search == True]), 1)
        self.assertGreaterEqual(len([req for req in requests if req.source_type == 'forums']),
            0)

        # The reported proportion should never decrease.
        self.assertTrue(all(x<=y for x, y in zip(proportions, proportions[1:])))
        ### TODO test handling 404 responses

    def test_scanresults_registered(self):
        c = Client()
        # Login.
        c.force_login(self.example_user)

        response = c.get('/scanresults', {'is_scan': True,
            'scan_query': 'test query phrase',
            'query_tags': [self.fun_tag.id, self.games_tag.id]
            })

        # NOTE since Celery doesn't have access to the test Django database, these scans will never
        # start.
        response_txt = response.content.decode('utf-8')
        self.assertIn('Scan status', response_txt)
        import pdb; pdb.set_trace()
        self.assertNotIn('Sorry!', response_txt)

    def test_guest_with_capacity(self):
        # Configure the system to always issue permissions.
        self.global_preferences['guest_scan_permissions_threshold'] = 0

        c = Client()

        response = c.get('/scanresults', {'is_scan': True,
            'scan_query': 'test query phrase',
            'query_tags': [self.fun_tag.id, self.games_tag.id]
            })

        response_txt = response.content.decode('utf-8')
        self.assertIn('Scan status', response_txt)
        self.assertNotIn('Sorry!', response_txt)

    def test_guest_no_capacity(self):
        # Configure the system to never issue permissions.
        self.global_preferences['guest_scan_permissions_threshold'] = (2 *
                self.global_preferences['concurrent_jobs_allowed'])

        c = Client()

        response = c.get('/scanresults', {'is_scan': True,
            'scan_query': 'test query phrase',
            'query_tags': [self.fun_tag.id, self.games_tag.id]
            })

        response_txt = response.content.decode('utf-8')
        self.assertNotIn('Scan status', response_txt)
        self.assertIn('Sorry!', response_txt)
