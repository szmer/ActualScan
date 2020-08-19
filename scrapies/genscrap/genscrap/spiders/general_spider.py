from datetime import datetime, timezone
import json
from json import JSONDecodeError
from logging import debug, info, error
import os, os.path
import time

import simpleflock
import scrapy

from redis import Redis
import redis_lock
from selenium import webdriver
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
from selenium.webdriver.common.keys import Keys

from scan.models import Site, ScanJob, ScrapeRequest
from asgiref.sync import sync_to_async

from genscrap.lib import (
        write_to_file, stractor_reading, timestamp_now, full_url, solr_update, update_request_status)

FILEDB_PATH = '/scrapies/pagecopies/'
# Note it's the original port, not the one mapped by docker-compose.
SOLR_NETLOC = 'solr:8983'
SOLR_PING_URL = 'http://solr:8983/solr/lookupy/admin/ping'
REQUEST_META = ['id', 'is_search', 'job_id', 'save_copies', 'site_name', 'site_url',
        'site_id', 'source_type']

#
# Selenium operation for search pages operating with JS.
#
redis = Redis(host='redis', port=6379, db=0, password=os.environ['REDIS_PASS'])
redis_lock.reset_all(redis)

async def get_js_search_pages(page_address, search_term, wait_time):
    """
    Return a list of HTML contents of JS search pages obtained with Selenium browser driver.

    We will give the page wait_time (in seconds) to load its results each time.
    """
    with redis_lock.Lock(redis, 'scrapy-selenium'):
        driver = webdriver.Remote(
                command_executor='http://selenium:4444/wd/hub',
                desired_capabilities=DesiredCapabilities.CHROME)
        driver.get(page_address)
        inputs = driver.find_elements_by_tag_name('input')
        search_inputs = []
        for input in inputs:
            if 'search' in input.get_attribute('class') or 'Search' in input.get_attribute('class'):
                search_inputs.append(input)
        if len(search_inputs) == 0:
            info('No search inputs found on {}'.format(page_address))
            return []
        elif len(search_inputs) > 1:
            info('Many potential search inputs found on {}'.format(page_address))
            return []
        else: # we have the one, good search input element
            search_pages = []
            elem = search_inputs[0]
            elem.clear()
            elem.send_keys(search_term)
            elem.send_keys(Keys.RETURN)
            time.sleep(wait_time)
            search_pages.append(driver.page_source)
            # TODO try to retrieve the next ones
            return search_pages

#
# The spider.
#
class GeneralSpider(scrapy.Spider):
    name = 'general'
    # We want to catch them to notify the DB of failure.
    handle_httpstatus_list = [400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413,
            414, 415, 416, 417, 500, 501, 502, 503, 504, 505]

    def __init__(self, selenium_wait_time=0, **kwargs):
        self.at_first_request = True
        self.selenium_wait_time = int(selenium_wait_time)
        super().__init__(self, **kwargs)

    def start_requests(self):
        # Start mock-pinging Solr and getting requests from DB.
        yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                errback=self.fail)

    async def fail(self, failure):
        """
        This is to be called as errback for Scrapy requests to log HTTP, DNS errors etc.
        """
        error('Scrapy error deferred to errback: '.format(repr(failure)))
        await self.fail_with_comment(repr(failure), request_id=failure.request.meta['id'])

    async def fail_with_comment(self, comment, db_request=False, request_id=False):
        # Update the ScrapeRequest if possible.
        if request_id:
            db_request = await sync_to_async(list)(ScrapeRequest.objects.filter(id=request_id).all())
            if db_request:
                db_request = db_request[0]
        if db_request:
            await sync_to_async(update_request_status)(db_request, 'failed',
                    failure_comment='Scrapy reported {}'.format(comment))

    def save_page_to_disk(self, db_path, url, page_html, meta):
        """
        Save the page to disk for manual inspection.
        """
        info('Saving a copy of {}'.format(url))
        with simpleflock.SimpleFlock(db_path+'index.csv.lock'):
            if os.path.isfile(db_path+'index.csv'):
                with open(db_path+'index.csv', 'r') as db_index_file:
                    entry_n = len(db_index_file.readlines())
            else:
                entry_n = 0
            db_catalog = db_path + str(entry_n) + '/'
            os.makedirs(db_catalog, exist_ok=False)
            write_to_file(db_catalog+'index', page_html)
            write_to_file(db_catalog+'url', url)
            write_to_file(db_catalog+'access-timestamp', timestamp_now())
            for key, value in meta.items():
                write_to_file(db_catalog+key, value)
            # Append the db entry for this page.
            with open(db_path+'index.csv', 'a+') as db_index_file:
                print(entry_n, url, file=db_index_file)

    async def monitoring_parse(self, response):
        """
        Scrapy parse function for Solr dummy monitoring requests - we want to search Postgres for
        new requests.

        Due to the Redis lock it should be safe to call multiplw such requests at once, although
        Scrapy will limit requesrs for the dummy Solr domain.
        """
        lock = redis_lock.Lock(redis, 'scrapy-monitoring-parse')
        if lock.acquire():
            if self.at_first_request:
                info('Renewing scheduled and ran requests not committed...')
                scrape_requests_left = await sync_to_async(list)(ScrapeRequest.objects.filter(
                    status__in=['scheduled', 'ran'], site_type='web').all())
                for scrape_request in scrape_requests_left:
                    await sync_to_async(update_request_status)(scrape_request, 'waiting')
                info('{} requests renewed.'.format(len(scrape_requests_left)))
                self.at_first_request = False
            scrape_requests_waiting = await sync_to_async(list)(ScrapeRequest.objects.filter(
                status='waiting', site_type='web').all())
            debug('{} waiting requests to be made'.format(len(scrape_requests_waiting)))
            for scrape_request in scrape_requests_waiting:
                await sync_to_async(update_request_status)(scrape_request, 'scheduled')
                meta = {attr: getattr(scrape_request, attr)
                            for attr
                            in REQUEST_META}
                url = full_url(scrape_request.target, scrape_request.site_url)
                yield scrapy.Request(url=url, callback=self.parse, meta=meta, errback=self.fail,
                        priority=5 if 'is_search' in meta and meta['is_search'] else 0)
            lock.release() # NOTE on some failure above the lock won't be released and the spider wil finish
            yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                    errback=self.fail)

    async def interpret_stractor(self, source_type, html_text, request_id):
        """
        Interpret the html_text on source_type with Speechtractor. Indicate failure in request_id.
        Return the object loaded from response JSON.
        """
        stractor_output = stractor_reading(html_text, source_type)
        # Speechtractor failures.
        if isinstance(stractor_output, int):
            await self.fail_with_comment(
                    'Could not reach Speechtractor, status code {}'.format(stractor_output),
                    request_id=request_id)
            return None
        return json.loads(stractor_output)

    async def start_requests_from_search(self, response, stractor_response_json):
        for found_page in stractor_response_json:
            if 'url' in found_page:
                is_page_search = (True
                        if 'is_search' in found_page and found_page['is_search']
                        else False)
                scrape_request = await sync_to_async(ScrapeRequest.objects.create)(
                        target=found_page['url'],
                        is_search=is_page_search,
                        job_id=response.meta['job_id'],
                        status='scheduled',
                        status_changed=datetime.now(timezone.utc),
                        source_type=response.meta['source_type'],
                        site_name=response.meta['site_name'],
                        site_type='web',
                        site_url=response.meta['site_url'],
                        site_id=response.meta['site_id'],
                        save_copies=response.meta['save_copies'])
                debug('Created a scrape request for {} from search, for {}'.format(
                    found_page['url'], response.meta['job_id']))
                yield response.follow(found_page['url'],
                        callback=self.parse,
                        meta={attr: getattr(scrape_request, attr) for attr
                            in REQUEST_META},
                        priority=5 if is_page_search else 0)

    async def parse(self, response):
        """
        The "regular" Scrapy parse function for search pages and indexed pages. The former should
        yield some new Scrapy Requests to follow.
        """
        # We go through a list instead of a get() to be able to async the query relatively easily.
        db_request = await sync_to_async(list)(
                ScrapeRequest.objects.filter(id=response.meta['id']).all())
        db_request = db_request[0]
        if db_request.status == 'cancelled':
            return
        # Update the status of the request (ran, failed).
        if response.status != 200:
            await self.fail_with_comment('Status code: {}'.format(response.status),
                    db_request=db_request)
            return

        lock = redis_lock.Lock(redis, 'scrapy-monitoring-parse')
        if not lock.locked():
            yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                    errback=self.fail)

        # Interpret the webpage with speechtractor.
        if response.meta['is_search']:
           source_type = 'searchpage'
        else:
            source_type = response.meta['source_type']
        try:
            stractor_response_json = await self.interpret_stractor(source_type, response.text,
                    response.meta['id'])
        except JSONDecodeError:
            stractor_response_json = None

        # Handling search pages.
        if response.meta['is_search']:
            # If we have a good response with pages.
            if stractor_response_json is not None and stractor_response_json:
                await self.start_requests_from_search(response, stractor_response_json)
                await sync_to_async(update_request_status)(db_request, 'committed')
            # On empty response, try the JS search extraction with Selenium.
            elif stractor_response_json is not None: # reachable but empty
                job_objs = await sync_to_async(list)(ScanJob.objects.filter(
                    id=response.meta['job_id']).all())
                query_phrase = job_objs[0].query_phrase
                selenium_error = False
                try:
                    js_search_pages = await get_js_search_pages(response.url,
                            query_phrase, self.selenium_wait_time)
                except Exception as e:
                    js_search_pages = False
                    selenium_error = e
                if js_search_pages:
                    # TODO handle many pages, maybe in separate requests?
                    for page_n, search_page in enumerate(js_search_pages):
                        if response.meta['save_copies']:
                            self.save_page_to_disk(
                                    FILEDB_PATH, response.url+'___SEARCH_{}'.format(page_n),
                                    search_page,
                                    response.meta)
                        try:
                            stractor_response_json = await self.interpret_stractor('searchpage',
                                    search_page, response.meta['id'])
                        except JSONDecodeError:
                            stractor_response_json = None
                        if stractor_response_json is not None and stractor_response_json:
                            await self.start_requests_from_search(response, stractor_response_json)
                            await sync_to_async(update_request_status)(db_request, 'committed')
                        else:
                            info('Empty Speechtractor reponse for JS extraction from: {}'.format(
                               response.url))
                            await self.fail_with_comment(
                                    'Speechtractor empty response for JS extraction',
                                    db_request=db_request)
                else:
                    if selenium_error:
                        info('Selenium error {} for search target: {}'.format(selenium_error,
                            response.url))
                        await self.fail_with_comment('selenium error {}'.format(selenium_error),
                                db_request=db_request)
                    else:
                        info('JS search extraction failed for search target: {}'.format(response.url))
                        await self.fail_with_comment('JS search extraction failed',
                                db_request=db_request)
            # If we cannot reach Speechtractor.
            else:
                info('Unable to get Speechtractor response: {} for search target: {}'.format(
                    stractor_response_json, response.url))
                await self.fail_with_comment('Speechtractor unreachable',
                        db_request=db_request)
        # The actual page indexing.
        else:
            if stractor_response_json is not None and stractor_response_json:
                site_obj = await sync_to_async(list)(Site.objects.filter(
                    id=response.meta['site_id']).all())
                site_obj = site_obj[0]
                # Get the site tags.
                # We select_related to avoid Django complaining about getting tags in async.
                site_tag_links = await sync_to_async(list)(site_obj.tag_links.select_related(
                    'tag').all())
                site_tags = [tag_link.tag.name for tag_link in site_tag_links]
                for doc in stractor_response_json:
                    doc['date_retr'] = timestamp_now()
                    doc['site_name'] = response.meta['site_name']
                    doc['source_type'] = response.meta['source_type']
                    if len(stractor_response_json) == 1:
                        doc['real_doc'] = 'self'
                        doc['url'] = response.url
                    else:
                        doc['real_doc'] = response.url
                    doc['url'] = full_url(doc['url'], response.meta['site_url'])
                    doc['tags'] = site_tags
                    doc['reason_scraped'] = response.meta['job_id']
                # This sends documents to solr with default settings. We need to do a manual commit
                # afterwards.
                solr_json_text = json.dumps(stractor_response_json)
                solr_update(solr_json_text, req_id=response.meta['id'],
                        req_class=ScrapeRequest)
                # Do the manual commit.
                # TODO commit one doc in one go with {add: {doc:}} json structure
                solr_update('{"commit": {}}', req_id=response.meta['id'],
                        req_class=ScrapeRequest)
                await sync_to_async(update_request_status)(db_request, 'committed')
            else:
                info('Got empty/none Speechractor response: {} for scraping target: {}'.format(
                    stractor_response_json, response.url))
                await self.fail_with_comment('empty Speechtractor response or unreachable',
                        db_request=db_request)

        if response.meta['save_copies']:
            # Save the page.
            self.save_page_to_disk(FILEDB_PATH, response.url, response.text, response.meta)
