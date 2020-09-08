from datetime import datetime, timezone
import json
from json import JSONDecodeError
import logging
from logging import getLogger
import os, os.path
import re
import time

import simpleflock
import scrapy

from redis import Redis
import redis_lock
from selenium import webdriver
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import ElementClickInterceptedException

from scan.models import Site, ScanJob, ScrapeRequest
from asgiref.sync import sync_to_async
from dynamic_preferences.registries import global_preferences_registry

from genscrap.lib import (
        write_to_file, stractor_reading, timestamp_now, full_url, solr_update, solr_check_urls,
        update_request_status
        )

FILEDB_PATH = '/scrapies/pagecopies/'
# Note it's the original port, not the one mapped by docker-compose.
SOLR_NETLOC = 'solr:8983'
SOLR_PING_URL = 'http://solr:8983/solr/{}/admin/ping'.format(os.environ['SOLR_CORE'])
REQUEST_META = ['id', 'is_search', 'job_id', 'save_copies', 'site_name', 'site_url',
        'site_id', 'source_type']

logger = getLogger('ascan_scrapy')

# Decrease the level of redis_lock to skip the acquire/release messages on DEBUG.
getLogger('redis_lock').setLevel(logging.INFO)
getLogger('protego').setLevel(logging.WARNING)
# Silence the Selenium driver logging.
from selenium.webdriver.remote.remote_connection import LOGGER
LOGGER.setLevel(logging.WARNING)

#
# Selenium operation for search pages operating with JS.
#
redis = Redis(host='redis', port=6379, db=0, password=os.environ['REDIS_PASS'])
redis_lock.reset_all(redis)

def extract_overlay_info(click_intercepted_msg):
    info = dict()
    class_match = re.search('would receive the click: <.* class="(?P<class>[^"]*)".*>',
            click_intercepted_msg)
    if class_match:
        info['class'] = class_match.group('class')
    id_match = re.search('would receive the click: <.* id="(?P<id>[^"]*)".*>',
            click_intercepted_msg)
    if id_match:
        info['id'] = id_match.group('id')
    return info

async def get_js_search_pages(page_address, search_term, wait_time, max_tried_next_elems,
        click_search_depth, infis_search_depth):
    """
    Return a list of HTML contents of JS search pages obtained with Selenium browser driver.

    wait_time - how much time to give Selenium after each click/scroll/enter press
    max_tried_next_elems - how many elements that look like a next-page button to try (each takes
       time!)
    click_search_depth - how many search pages to collect at maximum with next-page clicking
    infis_search_depth - how many search pages to collect at maximum with engaging in infinite scroll

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
            logger.debug('No search inputs found on {}'.format(page_address))
            return []
        elif len(search_inputs) > 1:
            logger.debug('Many potential search inputs found on {}'.format(page_address))
            return []
        else: # we have the one, good search input element
            search_pages = []
            elem = search_inputs[0]
            elem.clear()
            elem.send_keys(search_term)
            elem.send_keys(Keys.RETURN)
            time.sleep(wait_time)
            search_pages.append(driver.page_source)

            # Try to retrieve the next ones.
            # The "next button" method.
            clickable_next_elems = dict()
            for next_elem_str in ['next', 'Next']:
                elements = driver.find_elements_by_xpath(
                        "//*[normalize-space(text())='{}']".format(next_elem_str))
                if elements is not None:
                    logger.debug('Found {} next-page-like elements on {}/{} with "{}"'.format(
                        len(elements), page_address, search_term, next_elem_str))
                    clickable_next_elems[next_elem_str] = elements
            if clickable_next_elems:
                tried_elements_n = 0
                # Each next_elem_str can potentially give us many elements to try to click on. We
                # have to be careful to click at the same element each time if we are getting next
                # pages.
                for element_str, elements in clickable_next_elems.items():
                    # The outer loop break condition for breaking when the max exceeded.
                    if tried_elements_n >= max_tried_next_elems:
                        break
                    if len(search_pages) > 1:
                        break
                    # Try the elements found with this text, remembering their number on the found
                    # list.
                    for el_n, element in enumerate(elements):
                        tried_elements_n += 1
                        if tried_elements_n >= max_tried_next_elems: # exit the inner loop
                            break
                        if len(search_pages) > 1:
                            break
                        for click_n in range(click_search_depth):
                            prev_content_len = len(driver.page_source)
                            while True:
                                try:
                                    # Re-get the elements with the "next" string and see if the
                                    # number is the same as at the start.
                                    # (Also beware of Selenium declaring object refs "stale").
                                    current_elements = driver.find_elements_by_xpath(
                                            "//*[normalize-space(text())='{}']".format(
                                                element_str))
                                    # If the number is the same, presumably we can get the same
                                    # button/element at the same index.
                                    if len(current_elements) != len(elements):
                                        break
                                    current_element = current_elements[el_n]
                                    driver.execute_script(
                                            "return arguments[0].scrollIntoView(false);",
                                            current_element)
                                    current_element.click()
                                    break
                                # Remove the junk (cookie consents, mail signups etc.) from the
                                # front.
                                except ElementClickInterceptedException as e:
                                    deleted_overlay = False
                                    logger.debug('Trying to delete the overlay...')
                                    overlay_info = extract_overlay_info(e.msg)
                                    logger.debug('with {} -> {}'.format(e.msg, overlay_info))
                                    if 'id' in overlay_info:
                                        over_elem = driver.find_element_by_id(overlay_info['id'])
                                        logger.debug('Requested the elem by id')
                                        driver.execute_script('arguments[0].remove()', over_elem)
                                        logger.debug('Elem should be removed')
                                        deleted_overlay = True
                                    if not deleted_overlay and 'class' in overlay_info:
                                        over_elems = driver.find_elements_by_xpath(
                                                '//*[@class="{}"]'.format(overlay_info['class']))
                                        logger.debug('Requested elems by class')
                                        if len(over_elems) > max_tried_next_elems * 50:
                                            break
                                        for elem in over_elems:
                                            driver.execute_script('arguments[0].remove()', elem)
                                        logger.debug('Elems should be removed if any')
                                    if (not overlay_info
                                            or prev_content_len != len(driver.page_source)):
                                        break
                                    # Update prev_content_len to reflect the changes made now.
                                    prev_content_len = len(driver.page_source)
                            time.sleep(wait_time)
                            if len(driver.page_source) != prev_content_len:
                                search_pages.append(driver.page_source)
                            else:
                                break
            # The "scroll more" method for infinite scrolls.
            if len(search_pages) <= 1:
                body = driver.find_element_by_tag_name('body')
                for scroll_stage_n in range(infis_search_depth):
                    prev_content_len = len(driver.page_source)
                    body.send_keys(Keys.END)
                    time.sleep(wait_time)
                    if len(driver.page_source) > prev_content_len:
                        search_pages.append(driver.page_source)
                    else:
                        break
            return search_pages

#
# Load the Django preferences.
#
logger.info('Scrapy loading ActualScan configuration.')
global_preferences = global_preferences_registry.manager()
DEDUP_DATE_POST_CHECK = global_preferences['scanning__dedup_date_post_check']
DEDUP_DATE_RETR_CHECK = global_preferences['scanning__dedup_date_retr_check']
SELENIUM_WAIT_TIME = global_preferences['scanning__selenium_wait_time']
SELENIUM_MAX_CLICK_ELEMS_TRIED = global_preferences['scanning__selenium_max_click_elems_tried']
NORMAL_SEARCH_DEPTH = global_preferences['scanning__web_normal_search_depth']
JS_SEARCH_DEPTH = global_preferences['scanning__web_js_search_depth']
INFISCROLL_SEARCH_DEPTH = global_preferences['scanning__web_infiscroll_search_depth']

#
# The spider.
#
class GeneralSpider(scrapy.Spider):
    name = 'general'
    # We want to catch them to notify the DB of failure.
    handle_httpstatus_list = [400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413,
            414, 415, 416, 417, 500, 501, 502, 503, 504, 505]

    def __init__(self, **kwargs):
        self.dedup_date_post_check = DEDUP_DATE_POST_CHECK
        self.dedup_date_retr_check = DEDUP_DATE_RETR_CHECK
        self.selenium_wait_time = SELENIUM_WAIT_TIME
        self.selenium_max_click_elems_tried = SELENIUM_MAX_CLICK_ELEMS_TRIED 
        self.normal_search_depth = NORMAL_SEARCH_DEPTH # NOTE not implemented
        self.js_search_depth = JS_SEARCH_DEPTH
        self.infiscroll_search_depth = INFISCROLL_SEARCH_DEPTH
        super().__init__(self, **kwargs)

        logger.info('Renewing scheduled and ran requests not committed...')
        cancelled_count = ScrapeRequest.objects.filter(status__in=['scheduled', 'ran', 'waiting'],
                site_type='web', job__status='terminated').update(status='cancelled', 
                        status_changed=datetime.now(timezone.utc))
        logger.info('{} requests cancelled (terminated jobs).'.format(cancelled_count))
        renewed_count = ScrapeRequest.objects.filter(status__in=['scheduled', 'ran'],
                site_type='web').update(status='waiting', status_changed=datetime.now(timezone.utc))
        logger.info('{} requests renewed.'.format(renewed_count))

    async def fail(self, failure):
        """
        This is to be called as errback for Scrapy requests to log HTTP, DNS errors etc.
        """
        logger.error('Scrapy error deferred to errback: '.format(repr(failure)))
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
        logger.info('Saving a copy of {}'.format(url))
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

    async def requests_from_search(self, response, stractor_response_json, lead_count=0):
        reqs = []
        # We should skip the requests before trying to download them for that to make sense.
        urls_to_request = [doc['url'] for doc in stractor_response_json if 'url' in doc]
        urls_to_skip = solr_check_urls(self.dedup_date_post_check, self.dedup_date_retr_check,
                urls_to_request)
        for found_page in stractor_response_json:
            if 'url' in found_page and not found_page['url'] in urls_to_skip:
                is_page_search = (True
                        if 'is_search' in found_page and found_page['is_search']
                        else False)
                scrape_request = await sync_to_async(ScrapeRequest.objects.create)(
                        target=found_page['url'],
                        is_search=is_page_search,
                        job_id=response.meta['job_id'],
                        lead_count=lead_count if is_page_search else 0,
                        status='scheduled',
                        status_changed=datetime.now(timezone.utc),
                        source_type=response.meta['source_type'],
                        site_name=response.meta['site_name'],
                        site_type='web',
                        site_url=response.meta['site_url'],
                        site_id=response.meta['site_id'],
                        save_copies=response.meta['save_copies'])
                logger.debug('Created a scrape request for {} from search, for {}'.format(
                    found_page['url'], response.meta['job_id']))
                reqs.append(response.follow(found_page['url'],
                    callback=self.parse,
                    meta={attr: getattr(scrape_request, attr) for attr
                        in REQUEST_META},
                    priority=5 if is_page_search else 1))
        return reqs

    def start_requests(self):
        # Start mock-pinging Solr and getting requests from DB.
        yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                errback=self.fail)

    async def monitoring_parse(self, response):
        """
        Scrapy parse function for Solr dummy monitoring requests - we want to search Postgres for
        new requests.

        Due to the Redis lock it should be safe to call multiplw such requests at once, although
        Scrapy will limit requesrs for the dummy Solr domain.
        """
        logger.debug('A monitoring pagse')
        # NOTE this is also verbatim in normal parse(), see the explanation there
        lock = redis_lock.Lock(redis, 'scrapy-monitoring-parse')
        if lock.acquire():
            scrape_requests_waiting = await sync_to_async(list)(ScrapeRequest.objects.filter(
                status='waiting', site_type='web').order_by('-is_search')[:100])
            logger.debug('{} waiting requests to be made'.format(len(scrape_requests_waiting)))
            for scrape_request in scrape_requests_waiting:
                await sync_to_async(update_request_status)(scrape_request, 'scheduled')
                meta = {attr: getattr(scrape_request, attr)
                            for attr
                            in REQUEST_META}
                url = full_url(scrape_request.target, scrape_request.site_url)
                #logger.debug('Sending the waiting request for {}'.format(url))
                # Make sure that even the ordinary requests have higher priority than monitoring.
                yield scrapy.Request(url=url, callback=self.parse, meta=meta, errback=self.fail,
                        priority=5 if 'is_search' in meta and meta['is_search'] else 1)
            # NOTE on some failure above the lock won't be released and the spider wil finish
            lock.release()
            yield scrapy.Request(url=SOLR_PING_URL, callback=self.monitoring_parse,
                    errback=self.fail)

    async def parse(self, response):
        """
        The "regular" Scrapy parse function for search pages and indexed pages. The former should
        yield some new Scrapy Requests to follow.
        """
        logger.debug('Parsing the response for {}'.format(response.url))
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
        else:
            await sync_to_async(update_request_status)(db_request, 'ran')

        # NOTE this is copied from the monitoring_parse. We need to set "monitoring parses" to
        # lesser priority to let the normal parses through, but then we still need to monitor the DB.
        lock = redis_lock.Lock(redis, 'scrapy-monitoring-parse')
        if lock.acquire():
            scrape_requests_waiting = await sync_to_async(list)(ScrapeRequest.objects.filter(
                # NOTE we take less request to queue than monitor parses
                status='waiting', site_type='web').order_by('-is_search')[:10])
            logger.debug('{} waiting requests to be made'.format(len(scrape_requests_waiting)))
            for scrape_request in scrape_requests_waiting:
                await sync_to_async(update_request_status)(scrape_request, 'scheduled')
                meta = {attr: getattr(scrape_request, attr)
                            for attr
                            in REQUEST_META}
                url = full_url(scrape_request.target, scrape_request.site_url)
                # Make sure that even the ordinary requests have higher priority than monitoring.
                yield scrapy.Request(url=url, callback=self.parse, meta=meta, errback=self.fail,
                        priority=5 if 'is_search' in meta and meta['is_search'] else 1)
            # NOTE on some failure above the lock won't be released and the spider wil finish
            lock.release()

        # Cancel requests exceeding the maximum search depth.
        if (response.meta['is_search']
                and db_request.lead_count > global_preferences['scanning__web_normal_search_depth']):
            await sync_to_async(update_request_status)(db_request, 'cancelled',
                    failure_comment='exceeded web search depth')
            return

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
                reqs = await self.requests_from_search(response, stractor_response_json,
                        # increment the lead count for subsequent pages
                        lead_count=db_request.lead_count+1)
                # Cancel the scrape requests for the same search site.
                ScrapeRequest.objects.filter(target=db_request.target).exclude(
                        id=db_request.id).update(status='cancelled',
                                failure_comment='duplicate search page',
                                status_changed=datetime.now(timezone.utc))
                for req in reqs:
                    if req.meta['is_search']: # stop other duplicate searches
                        earlier_requests = await sync_to_async(list)(ScrapeRequest.objects.filter(
                            status='committed', is_search=True, target=req.url))
                        if len(earlier_requests):
                            ScrapeRequest.objects.filter(target=req.url).update(status='cancelled',
                                            failure_comment='duplicate search page',
                                            status_changed=datetime.now(timezone.utc))
                            continue
                    if req.url != db_request.target:
                        yield req
                await sync_to_async(update_request_status)(db_request, 'committed')
            # On empty response, try the JS search extraction with Selenium.
            elif stractor_response_json is not None: # reachable but empty
                job_objs = await sync_to_async(list)(ScanJob.objects.filter(
                    id=response.meta['job_id']).all())
                query_phrase = job_objs[0].query_phrase
                selenium_error = False
                try:
                    js_search_pages = await get_js_search_pages(response.url, query_phrase,
                            self.selenium_wait_time, self.selenium_max_click_elems_tried,
                            self.js_search_depth, self.infiscroll_search_depth)
                except Exception as e:
                    js_search_pages = False
                    selenium_error = e
                if js_search_pages:
                    search_pages_count = len(js_search_pages)
                    search_pages_fail_count = 0
                    # Store permalinks already processed to avoid duplicate requests for pages
                    # from infinite scrolls.
                    used_permalinks = set()
                    # Update the lead count - tell how many subordinate search pages we found.
                    db_request.lead_count = search_pages_count - 1
                    await sync_to_async(db_request.save)()
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
                            # Remove possible leftovers from previous stages of infinite scroll.
                            idxs_to_remove = set()
                            for doc_n, doc in enumerate(stractor_response_json):
                                if not 'url' in doc:
                                    idxs_to_remove.add(doc_n)
                                elif not doc['url'] in used_permalinks:
                                    used_permalinks.add(doc['url'])
                                else:
                                    idxs_to_remove.add(doc_n)
                            stractor_response_json = [doc for (doc_n, doc)
                                    in enumerate(stractor_response_json)
                                    if not doc_n in idxs_to_remove]
                            # Run the requests from search.
                            reqs = await self.requests_from_search(response, stractor_response_json)
                            for req in reqs:
                                yield req
                            await sync_to_async(update_request_status)(db_request, 'committed')
                        else:
                            search_pages_fail_count += 1
                    if search_pages_fail_count:
                        # NOTE we fail even with only some Speechtractor responses being empty!
                        logger.info('Empty Speechtractor reponse for JS extraction of {}/{} pages from:'
                                ' {}'.format(search_pages_fail_count, search_pages_count,
                                    response.url))
                        await self.fail_with_comment(
                                'Speechtractor empty response for JS extraction'
                                ' ({}/{} pages)'.format(search_pages_fail_count, search_pages_count),
                                db_request=db_request)
                else:
                    if selenium_error:
                        logger.info('Selenium error {} for search target: {}'.format(selenium_error,
                            response.url))
                        await self.fail_with_comment('selenium error {}'.format(selenium_error),
                                db_request=db_request)
                    else:
                        logger.info('JS search extraction failed for search target: {}'.format(response.url))
                        await self.fail_with_comment('JS search extraction failed',
                                db_request=db_request)
            # If we cannot reach Speechtractor.
            else:
                logger.info('Unable to get Speechtractor response: {} for search target: {}'.format(
                    stractor_response_json, response.url))
                await self.fail_with_comment('Speechtractor unreachable',
                        db_request=db_request)
        # The actual page indexing.
        else:
            if stractor_response_json is not None and stractor_response_json:
                site_obj = await sync_to_async(list)(Site.objects.filter(
                    id=response.meta['site_id']).all())
                site_obj = site_obj[0]
                for doc in stractor_response_json: # fill the missing fields
                    doc['date_retr'] = timestamp_now()
                    doc['site_name'] = response.meta['site_name']
                    doc['source_type'] = response.meta['source_type']
                    if len(stractor_response_json) == 1:
                        doc['real_doc'] = 'self'
                        doc['url'] = response.url
                    else:
                        doc['real_doc'] = response.url
                    doc['url'] = full_url(doc['url'], response.meta['site_url'])
                    doc['reason_scraped'] = response.meta['job_id']
                # This sends documents to solr with default settings. We need to do a manual commit
                # afterwards.
                solr_json_text = json.dumps(stractor_response_json)
                # We have to convert it to async to allow it to update the request on failure.
                await sync_to_async(solr_update)(solr_json_text, req_id=response.meta['id'],
                        req_class=ScrapeRequest)
                # Do the manual commit.
                # TODO commit one doc in one go with {add: {doc:}} json structure
                solr_update('{"commit": {}}', req_id=response.meta['id'],
                        req_class=ScrapeRequest)
                await sync_to_async(update_request_status)(db_request, 'committed')
            else:
                logger.info('Got empty/none Speechractor response: {}'
                        ' for scraping target: {}'.format(stractor_response_json, response.url))
                await self.fail_with_comment('empty Speechtractor response or unreachable',
                        db_request=db_request)

        if response.meta['save_copies']:
            # Save the page.
            self.save_page_to_disk(FILEDB_PATH, response.url, response.text, response.meta)
