from datetime import datetime, timedelta, timezone
from logging import error

from searchfront.extensions import db

from searchfront.blueprints.site import Tag
from searchfront.blueprints.scan_schedule import ScanJob, ScrapeRequest
from searchfront.blueprints.live_config import LiveConfigValue

def request_scan(user_id, query_phrase : str, query_tags : list, force_new=False):
    """
    Put an awaiting scan job in the database. query_tags should be a list of strings. If force_new
    is set and the job already exists, a ValueError is raised. Return the ScanJob object.
    """
    job_id = ScanJob.identifier(user_id, query_phrase, query_tags)
    job = ScanJob.query.get(job_id)
    if job is None:
        job = ScanJob(id=job_id, query_phrase=query_phrase, query_tags=query_tags, status='waiting')
        db.session.add(job)
        db.session.commit()
    elif force_new:
        raise ValueError('Scan job {} already exists, but was asked to recreate it'.format(job_id))
    return job

def start_scan(scan_job):
    """
    Start the scan_job (put the scrape requests).
    """
    phrase_tokens = scan_job.query_phrase.split()
    # TODO test for the situation with no coherent/findable tags
    tag_strs = scan_job.query_tags.split(',')
    tags = Tag.query.filter(Tag.name.in_(tag_strs))
    sites_queried = set()
    for tag in tags:
        for site in tag.sites:
            if not site in sites_queried:
                sites_queried.add(site) # a site can belong to multiple tags
                if site.site_type == 'web':
                    search_pointer = site.search_url_for(phrase_tokens)
                    req = ScrapeRequest(target=search_pointer, is_search=True, job_id=scan_job.id,
                            status='waiting',
                            source_type=site.source_type, site_name=site.site_name,
                            site_url=site.homepage_url, site_type=site.site_type,
                            site_id = site.id,
                            query_tags=scan_job.query_tags, save_copies=scan_job.save_copies)
                elif site.site_type == 'reddit':
                    req = ScrapeRequest(target='[reddit] '+scan_job.query_phrase,
                            is_search=True, job_id=scan_job.id,
                            status='waiting', site_type=site.site_type,
                            source_type=site.source_type, site_name=site.site_name,
                            site_url=site.homepage_url,
                            site_id = site.id,
                            query_tags=scan_job.query_tags, save_copies=scan_job.save_copies)
                else:
                    error('Unknown site type {} for site {} ({})'.format(site.site_type, site.id,
                        site.homepage_url))
                db.session.add(req)
                db.session.commit()
    scan_job.change_status('working')
    db.session.commit()
    # TODO failure

def terminate_scan(scan_job):
    """
    Terminate the scan job. All waiting scrape requests are cancelled; scheduled ones will still be
    made unless we restart Scrapy.
    """
    waiting_requests = [req for req in scan_job.requests if req.status == 'waiting']
    for req in waiting_requests:
        req.change_status('cancelled')
        db.session.commit()
    scan_job.change_status('terminated')
    db.session.commit()

def scan_progress_info(scan_job):
    """
    A dictionary: 'phase' ('waiting', 'search', 'crawl' or appropriate ScanJob status), possibly
    also 'fails', 'last_url', 'dl_proportion'. Note that dl_proportion reflects proportion of the
    pages to be crawled or the search pages depending on the phase.
    """
    if scan_job.status in ['waiting', 'finished', 'terminated', 'rejected']:
        return {'phase': scan_job.status}
    # TODO possibly optimize queries
    pending_search_count = ScrapeRequest.query.filter_by(
            status='waiting',
            is_search=True,
            job_id=scan_job.id
            ).count()
    pending_crawl_count = ScrapeRequest.query.filter_by(
        status='waiting',
        is_search=False,
        job_id=scan_job.id
        ).count()
    # This is the job of do_scan_management!
    #-if pending_crawl_count == pending_search_count == 0:
    #-    scan_job.change_status('finished')
    #-    db.session.commit()
    #-    return {'phase': 'done'}
    phase = ('search' if pending_search_count > 0 else 'crawl')
    done_requests = list(ScrapeRequest.query.filter(
        (ScrapeRequest.status == 'ran')
        | (ScrapeRequest.status == 'failed'),
        ScrapeRequest.job_id == scan_job.id
        ).order_by(ScrapeRequest.status_changed.desc()))
    fails = len([req for req in done_requests if req.status == 'failed'])
    if phase == 'search':
        done_count = len([req for req in done_requests if req.is_search])
        if (pending_search_count + done_count) > 0: # avoid division by 0
            dl_proportion = (pending_search_count
                    / (pending_search_count + done_count))
        else:
            dl_proportion = 1.0
    else:
        done_count = len([req for req in done_requests if not req.is_search])
        if (pending_crawl_count + done_count) > 0:
            dl_proportion = (pending_crawl_count
                    / (pending_crawl_count + done_count))
        else:
            dl_proportion = 1.0
    return {'phase': phase, 'fails': fails,
            'last_url': done_requests[0].target if len(done_requests) > 0 else None,
            'dl_proportion': dl_proportion}

def do_scan_management():
    """
    Returns a dictionary: {}.
    - Marks jobs as finished if all their scrape request have ran.
    - If there is spare capacity to run jobs, runs them.
    - Removes the old jobs and requests (as configured in live config).
    """
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
            job.change_status('finished')
    db.session.commit()
    # Run new jobs if possible.
    concurrent_jobs_allowed = int(LiveConfigValue.query.get('concurrent_jobs_allowed').value)
    spare_capacity = concurrent_jobs_allowed - ScanJob.query.filter_by(status='working').count()
    if spare_capacity > 0:
        # TODO an explicit queue? currently just a naive FIFO
        jobs_to_start = list(ScanJob.query.filter_by(status='waiting').order_by(
            ScanJob.status_changed.asc()).limit(spare_capacity))
        for job in jobs_to_start:
            start_scan(job)
        spare_capacity -= len(jobs_to_start)
    # Remove old jobs and requests.
    scan_job_time_to_live = int(LiveConfigValue.query.get('scan_job_time_to_live').value)
    scan_job_time_threshold = datetime.now(timezone.utc) - timedelta(
            seconds=scan_job_time_to_live)
    old_jobs_finished = list(ScanJob.query.filter(ScanJob.status=='working',
        ScanJob.status_changed < scan_job_time_threshold))
    for job in old_jobs_finished:
        db.session.delete(job)
    db.session.commit()
    scrape_request_time_to_live = int(LiveConfigValue.query.get(
        'scrape_request_time_to_live').value)
    scrape_request_time_threshold = datetime.now(timezone.utc) - timedelta(
            seconds=scrape_request_time_to_live)
    old_reqs_finished = list(ScrapeRequest.query.filter(ScrapeRequest.status=='working',
        ScrapeRequest.status_changed < scrape_request_time_threshold))
    for req in old_reqs_finished:
        db.session.delete(req)
    db.session.commit()
    return {'spare_capacity': spare_capacity}
