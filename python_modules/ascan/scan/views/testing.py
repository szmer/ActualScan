from datetime import datetime, timezone
import http.client
import json
from time import sleep
import urllib.parse

from django.contrib.admin.views.decorators import staff_member_required
from django.contrib.auth.models import User
from django.http import JsonResponse

from scan.models import Tag, Site, ScanJob, ScrapeRequest
from scan.control import (request_scan, terminate_scan,
        scan_progress_info)

class TestingFailure(Exception):
    pass

def solr_search_json(query):
    """
    Query Solr directly, independently of the omnivore infrastructure.
    """
    query_str = '/solr/lookupy/select?q=' + urllib.parse.quote(query, safe='\\')
    print(query_str)
    get_conn = http.client.HTTPConnection('solr', port=8983)
    get_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
    get_response = get_conn.getresponse()
    get_response_text = get_response.read().decode('utf-8')
    get_response_json = json.loads(get_response_text)
    return get_response_json

def solr_purge_site(site_name):
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
    if not del_response.status == 200:
        raise TestingFailure('del_response.status != 200')
    # Check the deletion.
    #solr_response_json = solr_search_json('site_name:{}'.format(site_name))
    #if not 'response' in solr_response_json:
    #    raise TestingFailure('"response" not in solr_response_json')
    #if not 'numFound' in solr_response_json['response']:
    #    raise TestingFailure('"numFound" not in solr_response_json["response"]')
    #if not solr_response_json['response']['numFound'] == 0:
    #    raise TestingFailure('solr_response_json["response"]["numFound"] != 0')

@staff_member_required
def testscanenv(request):
    response_object = { 'status': 'failure', 'stage': 'testing not started' }

    try:
        response_object['stage'] = 'adding tags'
        # Setup test tags and sites. NOTE We will always try to delete them later.
        fun_tag, _ = Tag.objects.get_or_create(name='funtest', level=100000,
                description='Sites containing fun things.')
        games_tag, _ = Tag.objects.get_or_create(name='gamestest', level=100000,
                description='Games of all kinds.')
        reddit_tag, _ = Tag.objects.get_or_create(name='reddittest', level=100000,
                description='Various subreddits.')
        response_object['stage'] = 'adding sites'
        quotes_site, _ = Site.objects.get_or_create(homepage_url='http://quotes.toscrape.com',
                level=100000,
                search_pointer='http://quotes.toscrape.com/tag/|||fat|||+|||cat|||/page/1',
                source_type='blog', site_name='quotes.toscrape.com',
                site_type='web')
        reddit_site, _ = Site.objects.get_or_create(homepage_url='https://reddit.com/r/test',
                level=100000,
                search_pointer='test',
                source_type='forums', site_name='/r/test',
                site_type='reddit')
        response_object['stage'] = 'adding tags to sites'
        quotes_site.tags.add(fun_tag)
        quotes_site.tags.add(games_tag)
        reddit_site.tags.add(fun_tag)
        reddit_site.tags.add(reddit_tag)
        # Add the example user.
        response_object['stage'] = 'creating the example user'
        example_user, _ = User.objects.get_or_create(username='example_username',
                email='john@example.com')
        example_user.set_password('example_pass')

        # The scan environment basic check.
        response_object['stage'] = 'contacting speechtractor'
        stractor_conn = http.client.HTTPConnection('speechtractor', port=3756)
        stractor_conn.request('GET', '/api/v01/status')
        stractor_response = stractor_conn.getresponse()
        if not stractor_response.status == 200:
            raise TestingFailure('stractor_response.status != 200')
        if not stractor_response.read() == b'ok':
            raise TestingFailure('stractor_response.read() != b"ok"')

        response_object['stage'] = 'starting the mock job for checking the environment'
        prescan_time = datetime.now(timezone.utc)
        job = request_scan(example_user.id, 'NabuchodonozorKopieJeftego?', ['funtest'],
                force_new=True)
        if not job.last_checked >= prescan_time:
            raise TestingFailure('job.last_checked < prescan_time')

        # this should start the scan, given that the celery task works.
        sleep(2.0)

        response_object['stage'] = 'checking the progress info'
        progress_info = scan_progress_info(job.id)
        if not 'phase' in progress_info:
            raise TestingFailure('not "phase" in progress_info')
        if not progress_info['phase'] in ['search', 'crawl', 'done']:
            raise TestingFailure('not progress_info["phase"] in ["search", "crawl", "done"] ({})'.
                    format(progress_info['phase']))
        response_object['stage'] = 'checking the sites included'
        reqs = ScrapeRequest.objects.filter(job=job).all()
        # (avoid request type cross-contamination)
        if not 'quotes.toscrape.com' in [req.site_name for req in reqs]:
            raise TestingFailure('not "quotes.toscrape.com" in [req.site_name for req in reqs]')
        if not '/r/test' in [req.site_name for req in reqs]:
            raise TestingFailure('not "/r/test" in [req.site_name for req in reqs])')

        # Terminate it immediately.
        response_object['stage'] = 'termination test'
        terminate_scan(job.id)
        job = ScanJob.objects.get(id=job.id)
        if not job.status == 'terminated':
            raise TestingFailure('job.status != "terminated"')
        if not len(job.requests.all()) > 0:
            raise TestingFailure('len(job.requests.all()) is 0')
        if not (set([req.status for req in job.requests.all()]) <=
                set(['ran', 'committed', 'failed', 'cancelled'])):
            raise TestingFailure('some requests rejected or terminated ({})'.format(
                set([req.status for req in job.requests.all()])))
        progress_info = scan_progress_info(job.id)
        if not progress_info['phase'] in ['terminated', 'finished']:
            raise TestingFailure('some requests were not terminated')

    except Exception as e:
        response_object['problem'] = repr(e)
        #fun_tag.delete()
        #games_tag.delete()
        #reddit_tag.delete()
        #quotes_site.delete()
        #reddit_site.delete()
        #example_user.delete()
        return JsonResponse(response_object)

    response_object['status'] = 'success'
    fun_tag.delete()
    games_tag.delete()
    reddit_tag.delete()
    quotes_site.delete()
    reddit_site.delete()
    example_user.delete()
    return JsonResponse(response_object)

@staff_member_required
def testscanweb(request):
    response_object = { 'status': 'failure', 'stage': 'testing not started' }

    try:
        response_object['stage'] = 'adding tags'
        # Setup test tags and sites. NOTE We will always try to delete them later.
        fun_tag, _ = Tag.objects.get_or_create(name='funtest', level=100000,
                description='Sites containing fun things.')
        games_tag, _ = Tag.objects.get_or_create(name='gamestest', level=100000,
                description='Games of all kinds.')
        reddit_tag, _ = Tag.objects.get_or_create(name='reddittest', level=100000,
                description='Various subreddits.')
        response_object['stage'] = 'adding sites'
        quotes_site, _ = Site.objects.get_or_create(homepage_url='http://quotes.toscrape.com',
                level=100000,
                search_pointer='http://quotes.toscrape.com/tag/|||fat|||+|||cat|||/page/1',
                source_type='blog', site_name='quotes.toscrape.com',
                site_type='web')
        reddit_site, _ = Site.objects.get_or_create(homepage_url='https://reddit.com/r/test',
                level=100000,
                search_pointer='test',
                source_type='forums', site_name='/r/test',
                site_type='reddit')
        response_object['stage'] = 'adding tags to sites'
        quotes_site.tags.add(fun_tag)
        quotes_site.tags.add(games_tag)
        reddit_site.tags.add(fun_tag)
        reddit_site.tags.add(reddit_tag)
        # Add the example user.
        response_object['stage'] = 'creating the example user'
        example_user, _ = User.objects.get_or_create(username='example_username',
                email='john@example.com')
        example_user.set_password('example_pass')

        response_object['stage'] = 'purging possible leftover documents from the site'
        solr_purge_site('quotes.toscrape.com')

        response_object['stage'] = 'requesting the scan'
        job = request_scan(example_user.id, 'inspirational', ['gamestest'],
                force_new=True)

        response_object['stage'] = 'waiting for the scan to start'
        # this should start the scan, given that the celery task works.
        sleep(1.5)

        # check the completion.
        response_object['stage'] = 'waiting for the scan progress'
        proportions = []
        # NOTE we currently need a long time due to toscrap.com redirections.
        for i in range(60*2): # wait 60 sec
            sleep(0.5)
            progress_info = scan_progress_info(job.id)
            if 'dl_proportion' in progress_info:
                proportions.append(progress_info['dl_proportion'])
            # We check requests status instead of Solr directly to avoid racing with updating those
            # statuses.
            requests_committed = ScrapeRequest.objects.filter(job_id=job.id,
                status='committed').all()
            if len(requests_committed) >= 6: # two search pages (prioritized), four crawls
                break
        terminate_scan(job.id)

        response_object['stage'] = 'examining Solr footprint of the scan'
        solr_response_json = solr_search_json('site_name:quotes.toscrape.com')
        if not 'response' in solr_response_json:
            raise TestingFailure('"response" not in solr_response_json')
        if not 'docs' in solr_response_json['response']:
            raise TestingFailure('"docs" not in solr_response_json["response"]')
        if not len(solr_response_json['response']['docs']) > 0:
            raise TestingFailure('len(solr_response_json["response"]["docs"]) == 0')
        # TODO ensure that we have site tags, and not query tags in the index!

        response_object['stage'] = 'examining produced requests'
        # We should also have the request for the second search page.
        if not len([req for req in requests_committed if req.is_search]) >= 2:
            raise TestingFailure('len([req for req in requests_committed if req.is_search])'
                    ' == {}, less than 2'.format(len([req for req in requests_committed
                        if req.is_search])))
        if not len([req for req in requests_committed if req.source_type == 'blog']) >= 2:
            raise TestingFailure('len([req for req in requests_committed if '
                    'req.source_type == "blog"]) == {}, less than 2'.format(
                        len([req for req in requests_committed if req.source_type == 'blog'])))

        # The reported proportion should never decrease.
        response_object['stage'] = 'examining the proportion sequence'
        if not all(x<=y for x, y in zip(proportions, proportions[1:])):
            raise TestingFailure('not an increasing proportion sequence: {}'.format(
                proportions))
        ### TODO test handling 404 responses

    except Exception as e:
        response_object['problem'] = repr(e)
        #fun_tag.delete()
        #games_tag.delete()
        #reddit_tag.delete()
        #quotes_site.delete()
        #reddit_site.delete()
        #example_user.delete()
        return JsonResponse(response_object)

    response_object['status'] = 'success'
    fun_tag.delete()
    games_tag.delete()
    reddit_tag.delete()
    quotes_site.delete()
    reddit_site.delete()
    example_user.delete()
    return JsonResponse(response_object)

@staff_member_required
def testscanreddit(request):
    response_object = { 'status': 'failure', 'stage': 'testing not started' }

    try:
        response_object['stage'] = 'adding tags'
        # Setup test tags and sites. NOTE We will always try to delete them later.
        fun_tag, _ = Tag.objects.get_or_create(name='funtest', level=100000,
                description='Sites containing fun things.')
        games_tag, _ = Tag.objects.get_or_create(name='gamestest', level=100000,
                description='Games of all kinds.')
        reddit_tag, _ = Tag.objects.get_or_create(name='reddittest', level=100000,
                description='Various subreddits.')
        response_object['stage'] = 'adding sites'
        quotes_site, _ = Site.objects.get_or_create(homepage_url='http://quotes.toscrape.com',
                level=100000,
                search_pointer='http://quotes.toscrape.com/tag/|||fat|||+|||cat|||/page/1',
                source_type='blog', site_name='quotes.toscrape.com',
                site_type='web')
        reddit_site, _ = Site.objects.get_or_create(homepage_url='https://reddit.com/r/test',
                level=100000,
                search_pointer='test',
                source_type='forums', site_name='/r/test',
                site_type='reddit')
        response_object['stage'] = 'adding tags to sites'
        quotes_site.tags.add(fun_tag)
        quotes_site.tags.add(games_tag)
        reddit_site.tags.add(fun_tag)
        reddit_site.tags.add(reddit_tag)
        # Add the example user.
        response_object['stage'] = 'creating the example user'
        example_user, _ = User.objects.get_or_create(username='example_username',
                email='john@example.com')
        example_user.set_password('example_pass')

        response_object['stage'] = 'purging possible leftover documents from the site'
        solr_purge_site('\\"'
                # NOTE This need not be url-encoded, but we have to double-escape backslashes to
                # make them reach Solr and actually escape the slashes. Otherwise we get something
                # like "a _text_ field doesn't exist" error.
                +'\\\\/r\\\\/test'
                +'\\"')

        response_object['stage'] = 'requesting the scan'
        job = request_scan(example_user.id, 'jour', ['reddittest'],
                force_new=True)

        response_object['stage'] = 'waiting for the scan to start'
        # this should start the scan, given that the celery task works.
        sleep(1.5)

        # check the completion.
        response_object['stage'] = 'waiting for the scan progress'
        proportions = []
        # NOTE we currently need a long time due to toscrap.com redirections.
        for i in range(60*2): # wait 60 sec
            sleep(0.5)
            progress_info = scan_progress_info(job.id)
            if 'dl_proportion' in progress_info:
                proportions.append(progress_info['dl_proportion'])
            # We check requests status instead of Solr directly to avoid racing with updating those
            # statuses.
            requests_committed = ScrapeRequest.objects.filter(job_id=job.id,
                status='committed').all()
            if len(requests_committed) >= 5:
                break
        terminate_scan(job.id)

        response_object['stage'] = 'examining Solr footprint of the scan'
        solr_response_json = solr_search_json('site_name:\\/r\\/test')
        if not 'response' in solr_response_json:
            raise TestingFailure('"response" not in solr_response_json')
        if not 'docs' in solr_response_json['response']:
            raise TestingFailure('"docs" not in solr_response_json["response"]')
        if not len(solr_response_json['response']['docs']) > 0:
            raise TestingFailure('len(solr_response_json["response"]["docs"]) == 0')
        # TODO ensure that we have site tags, and not query tags in the index!

        response_object['stage'] = 'examining produced requests'
        # Specifically, we cannot expect the search request to already commit (there may be more
        # submissions and it commits only after all sites in the Reddit scraper).
        requests = ScrapeRequest.objects.filter(job=job)
        if not len([req for req in requests if req.is_search]) == 1:
            raise TestingFailure('len([req for req in requests if req.is_search])'
                    ' == {}, not 1'.format(len([req for req in requests
                        if req.is_search])))
        if not len([req for req in requests if req.source_type == 'forums']) > 0:
            raise TestingFailure('len([req for req in requests if '
                    'req.source_type == "blog"]) == {}, non-0 wanted'.format(
                        len([req for req in requests if req.source_type == 'forums'])))

        # The reported proportion should never decrease.
        response_object['stage'] = 'examining the proportion sequence'
        if not all(x<=y for x, y in zip(proportions, proportions[1:])):
            raise TestingFailure('not an increasing proportion sequence: {}'.format(
                proportions))
        ### TODO test handling 404 responses

    except Exception as e:
        response_object['problem'] = repr(e)
        response_object['addr'] = '/solr/lookupy/select?q=' + urllib.parse.quote('site_name:\\/r\\/test', safe='\\')
        #fun_tag.delete()
        #games_tag.delete()
        #reddit_tag.delete()
        #quotes_site.delete()
        #reddit_site.delete()
        #example_user.delete()
        return JsonResponse(response_object)

    response_object['status'] = 'success'
    # NOTE this deletes the scan job by cascading and crashes the scraper!
    fun_tag.delete()
    games_tag.delete()
    reddit_tag.delete()
    quotes_site.delete()
    reddit_site.delete()
    example_user.delete()
    return JsonResponse(response_object)
