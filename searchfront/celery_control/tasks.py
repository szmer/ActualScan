from datetime import datetime, timedelta, timezone
import logging

from celery import Celery

from searchfront.app import create_app
from searchfront.extensions import db
from searchfront.blueprints.scan_schedule import ScanJob, ScrapeRequest
from searchfront.blueprints.scan_schedule.control import start_scan
from searchfront.blueprints.live_config import LiveConfigValue

celery_logger = logging.getLogger('searchfront_celery')

celery_app = Celery('searchfront')
app = create_app(settings_override={'DEBUG_TB_ENABLED': False})

celery_logger.addHandler(logging.StreamHandler())
celery_logger.setLevel(app.config['CELERY_LOG_LEVEL'])

@celery_app.task
def do_scan_management():
    """
    Returns a dictionary: {}.
    - Marks jobs as finished if all their scrape request have ran.
    - If there is spare capacity to run jobs, runs them.
    - Removes the old jobs and requests (as configured in live config).
    """
    with app.app_context():
        celery_logger.debug('Running scan management...')
        # Mark finished jobs.
        jobs_working = list(ScanJob.query.filter_by(status='working'))
        for job in jobs_working:
            requests_waiting = [req for req in job.requests if req.status in ['waiting', 'scheduled',
                'ran']]
            if len(requests_waiting) > 0:
                continue
            # We want to ensure that some requests are actually present even if finished to prevent
            # race conditions with jobs just being started.
            elif len(job.requests) > 0:
                celery_logger.info('Job {} marked as finished.'.format(job.id))
                job.change_status('finished')
        db.session.commit()
        # Run new jobs if possible.
        concurrent_jobs_allowed = int(LiveConfigValue.query.get('concurrent_jobs_allowed').value)
        spare_capacity = concurrent_jobs_allowed - ScanJob.query.filter_by(status='working').count()
        celery_logger.debug('There is {} of spare capacity'.format(spare_capacity))
        if spare_capacity > 0:
            # TODO an explicit queue? currently just a naive FIFO
            jobs_to_start = list(ScanJob.query.filter_by(status='waiting').order_by(
                ScanJob.status_changed.asc()).limit(spare_capacity))
            celery_logger.debug('{} awaiting jobs to start.'.format(len(jobs_to_start)))
            for job in jobs_to_start:
                celery_logger.info('Triggering start of the job {}.'.format(job.id))
                start_scan(job)
            spare_capacity -= len(jobs_to_start)
        # Remove old jobs and requests.
        scan_job_time_to_live = int(LiveConfigValue.query.get('scan_job_time_to_live').value)
        scan_job_time_threshold = datetime.now(timezone.utc) - timedelta(
                seconds=scan_job_time_to_live)
        old_jobs_finished = list(ScanJob.query.filter(ScanJob.status=='working',
            ScanJob.status_changed < scan_job_time_threshold))
        if len(old_jobs_finished) > 0:
            celery_logger.info('Removing {} old jobs.'.format(len(old_jobs_finished)))
        for job in old_jobs_finished:
            db.session.delete(job)
        db.session.commit()
        scrape_request_time_to_live = int(LiveConfigValue.query.get(
            'scrape_request_time_to_live').value)
        scrape_request_time_threshold = datetime.now(timezone.utc) - timedelta(
                seconds=scrape_request_time_to_live)
        old_reqs_finished = list(ScrapeRequest.query.filter(ScrapeRequest.status=='working',
            ScrapeRequest.status_changed < scrape_request_time_threshold))
        if len(old_reqs_finished) > 0:
            celery_logger.info('Removing {} old scrape reqs.'.format(len(old_reqs_finished)))
        for req in old_reqs_finished:
            db.session.delete(req)
        db.session.commit()
        return {'spare_capacity': spare_capacity}
