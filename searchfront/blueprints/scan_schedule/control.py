from datetime import datetime, timezone

from searchfront.extensions import db

from searchfront.blueprints.site.models import Tag
from searchfront.blueprints.scan_schedule.models import ScanJob, ScrapRequest

def request_scan(user_id, query_phrase, query_tags):
    job = ScanJob(id=ScanJob.identifier(user_id, query_phrase, query_tags),
            query_phrase=query_phrase, query_tags=query_tags,
            last_checked=datetime.now(timezone.utc), status='waiting')
    db.session.add(job)
    db.session.commit()

def start_scan(scan_job):
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
                req = ScrapRequest(url=search_url, is_search=True, job_id=scan_job.id,
                    status='waiting', status_changed=datetime.now(timezone.utc),
                    source_type=site.source_type,
                    save_copies=scan_job.save_copies)
                db.session.add(req)
                db.session.commit()
    scan_job.bump()
    scan_job.status = 'working'
    db.session.commit()
    # TODO failure

def scan_progress_info(scan_job):
    """
    A dictionary: 'phase' ('waiting', 'search', 'crawl' or 'done'), possibly also 'fails',
    'last_url', 'dl_proportion'. Note that dl_proportion reflects proportion of the pages to be
    crawled or the search pages depending on the phase. This may update the job to mark it as
    finished.
    """
    if scan_job.status == 'waiting':
        return {'phase': 'waiting'}
    if scan_job.status == 'finished':
        return {'phase': 'done'}
    if scan_job.status == 'terminated':
        return {'phase': 'terminated'}
    if scan_job.status == 'rejected':
        return {'phase': 'rejected'}
    # TODO possibly optimize queries
    pending_search_count = ScrapRequest.query.filter_by(
            status='waiting',
            is_search=True,
            job_id=scan_job.id
            ).count()
    pending_crawl_count = ScrapRequest.query.filter_by(
        status='waiting',
        is_search=False,
        job_id=scan_job.id
        ).count()
    # If everything is finished, mark it as such now.
    if pending_crawl_count == pending_search_count == 0:
        scan_job.status = 'finished'
        db.session.commit()
        return {'phase': 'done'}
    phase = ('search' if pending_search_count > 0 else 'crawl')
    done_requests = ScrapRequest.query.filter(
            (ScrapRequest.status == 'ran')
            | (ScrapRequest.status == 'failed'),
            ScrapRequest.job_id == scan_job.id
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
