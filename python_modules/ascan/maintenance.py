import argparse
from datetime import datetime, timedelta, timezone
import json
import logging
from logging import getLogger, debug, info
import os
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

from asgiref.sync import async_to_sync
import channels.layers
from dynamic_preferences.registries import global_preferences_registry

from scan.models import ScanJob, ScrapeRequest, ScanPermission, FeedbackPermission
from scan.control import start_scan, spare_scan_capacity, scan_progress_info

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
            seconds=global_preferences['scanning__guest_scan_permission_time_to_live'])
    # __lte is less than (<)
    ScanPermission.objects.filter(time_issued__lte=expiration_threshold).delete()
    # Similarly with the feedback permissions.
    expiration_threshold = datetime.now(timezone.utc) - timedelta(
            seconds=global_preferences['trust_levels__feedback_permission_time_to_exist'])
    FeedbackPermission.objects.filter(time_issued__lte=expiration_threshold).delete()
    # Mark finished jobs and broadcast progress to WebSockets.
    channel_layer = channels.layers.get_channel_layer()
    # The threshold to get scan jobs terminated due to timeout.
    scan_job_time_threshold = datetime.now(timezone.utc) - timedelta(
        seconds=global_preferences['scanning__scan_job_time_to_live'])
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
            seconds=global_preferences['scanning__scan_job_time_to_exist'])
    old_jobs_finished = ScanJob.objects.filter(status='working',
        status_changed__lte=scan_job_time_threshold)
    if len(old_jobs_finished) > 0:
        info('Removing {} old jobs.'.format(len(old_jobs_finished)))
    old_jobs_finished.delete()
    scrape_request_time_threshold = datetime.now(timezone.utc) - timedelta(
            seconds=global_preferences['scanning__scrape_request_time_to_exist'])
    old_reqs_finished = ScrapeRequest.objects.filter(status='working',
        status_changed__lte=scrape_request_time_threshold)
    if len(old_reqs_finished) > 0:
        info('Removing {} old scrape reqs.'.format(len(old_reqs_finished)))
    old_reqs_finished.delete()
    return {'spare_capacity': spare_capacity}

while True:
    do_scan_management()
    # Don't overwhelm the sockets and databases.
    time.sleep(2.0)
