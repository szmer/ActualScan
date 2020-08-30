import argparse
from datetime import datetime, timedelta, timezone
import http.client
import json
import logging
from logging import getLogger, debug, info, warning
import os
import socket
from threading import Thread
import time
#
# Read command line args.
#
argparser = argparse.ArgumentParser(description=
        'Run the maintenance script running ActualScan DB maintenance task in loop.')
argparser.add_argument('-L', '--loglevel', help='Logging level.')

args = argparser.parse_args()
logger = getLogger()
logging.basicConfig()
if args.loglevel:
    logger.setLevel(args.loglevel)
logger.info('Starting the maintenance loop.')

# Connect to Django.
import sys
sys.path.append('/ascan')
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'ascan.settings')
import django
django.setup()
from channels.routing import get_default_application
application = get_default_application()
from django.conf import settings

from asgiref.sync import async_to_sync
import channels.layers
from dynamic_preferences.registries import global_preferences_registry

from scan.models import ScanJob, ScrapeRequest, ScanPermission, FeedbackPermission
from scan.control import start_scan, spare_scan_capacity, scan_progress_info
from bg.models import AutocompleteTerm

TOP_TERMS = []
TOP_TERMS_N = -1

class SuggestionUpdatingThread(Thread):
    def run(self):
        global TOP_TERMS
        global TOP_TERMS_N
        if TOP_TERMS_N < 0 or not TOP_TERMS or TOP_TERMS_N >= len(TOP_TERMS):
            TOP_TERMS = []
            TOP_TERMS_N = 0
            global_preferences = global_preferences_registry.manager()
            query_str = ('/solr/{}/terms?terms.fl=text&terms.limit={}'.format(
                settings.SOLR_CORE,
                global_preferences['top_terms_with_autocomplete_phrases']))
            term_conn = http.client.HTTPConnection('solr', port=8983)
            term_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
            term_response = term_conn.getresponse()
            term_response_text = term_response.read().decode('utf-8')
            try:
                term_response_json = json.loads(term_response_text)
            except json.JSONDecodeError:
                warning('Bad JSON in Solr response to /terms request')
                return
            if not 'terms' in term_response_json or not 'text' in term_response_json['terms']:
                warning('Got unusable JSON from Solr response to /terms request: {}'.format(
                    term_response))
                return
            TOP_TERMS = []
            is_odd = True
            for value in term_response_json['terms']['text']:
                if is_odd:
                    TOP_TERMS.append(value)
                    is_odd = False
                else: # skip the frequencies
                    is_odd = True
            if not TOP_TERMS:
                debug('Got empty set of terms from Solr for autocomplete')
                return
        debug('Autocompletion term number {}'.format(TOP_TERMS_N))
        debug('Autocompletion terms {}'.format(TOP_TERMS))
        term = TOP_TERMS[TOP_TERMS_N]
        debug('Trying to get autocompletion phrases for {}'.format(term))
        TOP_TERMS_N += 1
        omnivore_addr = '/result?q={}'.format(term)
        omnivore_conn = http.client.HTTPConnection('omnivore', port=4242, timeout=60)
        omnivore_conn.request('GET', omnivore_addr, headers={'Content-type': 'application/json'})
        try:
            omnivore_response = omnivore_conn.getresponse()
        except socket.timeout:
            warning('Omnivore timeouted for autocomplete term {}'.format(term))
            return
        omnivore_response_text = omnivore_response.read()
        try:
            omnivore_results = json.loads(omnivore_response_text)
        except json.JSONDecodeError:
            warning('Bad omnivore response for {}: {}'.format(term, omnivore_response_text))
            return
        if not 'phrases' in omnivore_results or omnivore_results['phrases'] is None:
            debug('No phrases in omnivore results')
            return
        term_info = {'phrases': [phr['text'] for phr in omnivore_results['phrases'] if
            'text' in phr]}
        if not len(term_info['phrases']) and len(omnivore_results['phrases']):
            debug('Could not read the phrases info from JSON: {}'.format(omnivore_results['phrases']))
            return
        elif not len(term_info['phrases']):
            debug('No autocompletion phrases for {}'.format(term))
            return
        try:
            term_obj = AutocompleteTerm.objects.get(term=term)
            term_obj.suggest_data = term_info
            term_obj.save()
        except AutocompleteTerm.DoesNotExist:
            term_obj = AutocompleteTerm.objects.create(term=term, suggest_data=term_info)
        debug('Added autocompletion phrases for {}'.format(term))
        return

def do_scan_management():
    """
    Returns a dictionary: {}.
    - Marks jobs as finished if all their scrape request have ran.
    - If there is spare capacity to run jobs, runs them.
    - Removes the old jobs and requests (as configured in live config).
    """
    debug('Running scan management...')
    global_preferences = global_preferences_registry.manager()
    # Remove expired scan permissions.
    expiration_threshold = datetime.now(timezone.utc) - timedelta(
            seconds=global_preferences['guest_scan_permission_time_to_live'])
    # __lte is less than (<)
    ScanPermission.objects.filter(time_issued__lte=expiration_threshold).delete()
    # Similarly with the feedback permissions.
    expiration_threshold = datetime.now(timezone.utc) - timedelta(
            seconds=global_preferences['feedback_permission_time_to_exist'])
    FeedbackPermission.objects.filter(time_issued__lte=expiration_threshold).delete()
    # Mark finished jobs and broadcast progress to WebSockets.
    channel_layer = channels.layers.get_channel_layer()
    # The threshold to get scan jobs terminated due to timeout.
    scan_job_time_threshold = datetime.now(timezone.utc) - timedelta(
        seconds=global_preferences['scan_job_time_to_live'])
    debug('Jobs last changed before {} would be terminated now.'.format(scan_job_time_threshold))
    for job in ScanJob.objects.all(): # TODO filter old finished?
        if job.status == 'working':
            if job.status_changed < scan_job_time_threshold:
                info('Job {} marked as terminated (timeout).'.format(job.id))
                job.change_status('terminated')
            else:
                requests_waiting = job.requests.filter(status__in=
                        ['waiting', 'scheduled', 'ran']).count()
                if requests_waiting == 0:
                    info('Job {} marked as finished.'.format(job.id))
                    job.change_status('finished')
        if job.status != 'finished' or job.status_changed > scan_job_time_threshold:
            debug('Notifying sockets about the job {}'.format(job.id))
            try:
                async_to_sync(channel_layer.group_send)(
                        'scan_{}'.format(job.id), {
                            "type": 'progress.info',
                            "text": json.dumps({
                                'subject': 'progress_info',
                                'content': scan_progress_info(job.id)
                                }),
                            })
            except Exception as e:
                info('Websocket notification error: {}'.format(repr(e)))
    # Run new jobs if possible.
    spare_capacity = spare_scan_capacity()
    debug('There is {} of spare capacity'.format(spare_capacity))
    if spare_capacity > 0:
        # TODO an explicit queue? currently just a naive FIFO
        jobs_to_start = ScanJob.objects.filter(status='waiting').order_by(
            'status_changed')[:spare_capacity]
        debug('{} awaiting jobs to start.'.format(len(jobs_to_start)))
        for job in jobs_to_start:
            info('Triggering start of the job {}.'.format(job.id))
            start_scan(job)
        spare_capacity -= len(jobs_to_start)
    # Remove old jobs and requests.
    scan_job_time_threshold = datetime.now(timezone.utc) - timedelta(
            seconds=global_preferences['scan_job_time_to_exist'])
    old_jobs_finished = ScanJob.objects.filter(status='working',
        status_changed__lte=scan_job_time_threshold)
    if len(old_jobs_finished) > 0:
        info('Removing {} old jobs.'.format(len(old_jobs_finished)))
    old_jobs_finished.delete()
    scrape_request_time_threshold = datetime.now(timezone.utc) - timedelta(
            seconds=global_preferences['scrape_request_time_to_exist'])
    old_reqs_finished = ScrapeRequest.objects.filter(status='working',
        status_changed__lte=scrape_request_time_threshold)
    if len(old_reqs_finished) > 0:
        info('Removing {} old scrape reqs.'.format(len(old_reqs_finished)))
    old_reqs_finished.delete()
    return {'spare_capacity': spare_capacity}

autocomplete_thread = SuggestionUpdatingThread()
autocomplete_thread.run()
while True:
    do_scan_management()
    if not autocomplete_thread.is_alive():
        autocomplete_thread = SuggestionUpdatingThread()
        autocomplete_thread.run()
    # Don't overwhelm the sockets and databases.
    time.sleep(2.0)
