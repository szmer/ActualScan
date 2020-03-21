from datetime import datetime, timezone

from searchfront.extensions import db

from searchfront.blueprints.site import Tag
from searchfront.blueprints.scan_schedule import ScanJob, ScrapeRequest
from searchfront.blueprints.live_config import LiveConfigValue

def request_scan(user_id, query_phrase, query_tags):
    """
    Put an awaiting scan job in the database.
    """
    job = ScanJob(id=ScanJob.identifier(user_id, query_phrase, query_tags),
            query_phrase=query_phrase, query_tags=query_tags, status='waiting')
    db.session.add(job)
    db.session.commit()

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
                search_url = site.search_url_for(phrase_tokens)
                req = ScrapeRequest(url=search_url, is_search=True, job_id=scan_job.id,
                    status='waiting',
                    source_type=site.source_type, site_name=site.site_name,
                    site_url=site.homepage_url,
                    query_tags=scan_job.query_tags, save_copies=scan_job.save_copies)
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
    A dictionary: 'phase' ('waiting', 'search', 'crawl' or 'done'), possibly also 'fails',
    'last_url', 'dl_proportion'. Note that dl_proportion reflects proportion of the pages to be
    crawled or the search pages depending on the phase. This may update the job to mark it as
    finished.
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
    # If everything is finished, mark it as such now.
    if pending_crawl_count == pending_search_count == 0:
        scan_job.change_status('finished')
        db.session.commit()
        return {'phase': 'done'}
    phase = ('search' if pending_search_count > 0 else 'crawl')
    done_requests = ScrapeRequest.query.filter(
            (ScrapeRequest.status == 'ran')
            | (ScrapeRequest.status == 'failed'),
            ScrapeRequest.job_id == scan_job.id
            ).order_by('status_changed'.desc())
    fails = len([req for req in done_requests if req.status == 'failed'])
    if phase == 'search':
        dl_proportion = (pending_search_count
                / pending_search_count + len([req for req in done_requests if req.is_search]))
    else:
        dl_proportion = (pending_crawl_count
                / pending_crawl_count + len([req for req in done_requests if not req.is_search]))
    return {'phase': phase, 'fails': fails, 'last_url': done_requests[0].url,
            'dl_proportion': dl_proportion}

def do_scan_management():
    """
    Returns a dictionary: {}.
    - Marks jobs as finished if all their scrape request have ran.
    - If there is spare capacity to run jobs, runs them.
    - Removes the old jobs and requests (as configured in live config).
    """
    # Mark finished jobs.
    jobs_waiting = list(ScanJob.query.filter_by(status='working'))
    for job in jobs_waiting:
        requests_waiting = [req for req in job.requests if req.status in ['waiting', 'scheduled']]
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
            'status_changed'.asc()).limit(spare_capacity))
        for job in jobs_to_start:
            start_scan(job)
        spare_capacity -= len(jobs_to_start)
    # Remove old jobs and requests.
    scan_job_time_to_live = int(LiveConfigValue.query.get('scan_job_time_to_live').value)
    scan_job_time_threshold = datetime.now(timezone.utc) - datetime.timedelta(
            seconds=scan_job_time_to_live)
    old_jobs_finished = list(ScanJob.query.filter(ScanJob.status=='working',
        ScanJob.status_changed < scan_job_time_threshold))
    for job in old_jobs_finished:
        db.session.delete(job)
    db.session.commit()
    scrape_request_time_to_live = int(LiveConfigValue.query.get(
        'scrape_request_time_to_live').value)
    scrape_request_time_threshold = datetime.now(timezone.utc) - datetime.timedelta(
            seconds=scrape_request_time_to_live)
    old_reqs_finished = list(ScrapeRequest.query.filter(ScrapeRequest.status=='working',
        ScrapeRequest.status_changed < scrape_request_time_threshold))
    for req in old_reqs_finished:
        db.session.delete(req)
    db.session.commit()
    return {'spare_capacity': spare_capacity}
