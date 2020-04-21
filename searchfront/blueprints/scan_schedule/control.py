from flask import current_app
from sqlalchemy import func

from searchfront.extensions import db

from searchfront.blueprints.site import Tag
from searchfront.blueprints.scan_schedule import ScanJob, ScrapeRequest
from searchfront.blueprints.live_config import LiveConfigValue

def request_scan(user_id, query_phrase : str, query_tags : list, force_new=False):
    """
    Put an awaiting scan job in the database. query_tags should be a list of strings. If force_new
    is set and the job already exists, a ValueError is raised. Return the ScanJob object.
    """
    # TODO also recreate the job if in an inactive status
    query_tags_str = ','.join(query_tags)
    job_id = ScanJob.identifier(user_id, query_phrase, query_tags_str)
    job = ScanJob.query.get(job_id)
    if job is not None and force_new:
        db.session.delete(job) # should cascade to the scrape requests
        db.session.commit()
        job = None
    if job is None:
        job = ScanJob(id=job_id, query_phrase=query_phrase, query_tags=query_tags_str,
                status='waiting')
        db.session.add(job)
        db.session.commit()
    return job

def start_scan(scan_job):
    """
    Start the scan_job (put the scrape requests).
    """
    current_app.logger.info('Starting scan job {}'.format(scan_job.id))
    phrase_tokens = scan_job.query_phrase.split()
    # TODO test for the situation with no coherent/findable tags
    tag_strs = scan_job.query_tags.split(',')
    tags = Tag.query.filter(Tag.name.in_(tag_strs))
    sites_queried = set()
    website_count = 0
    subreddit_count = 0
    for tag in tags:
        for site in tag.sites:
            if not site in sites_queried:
                sites_queried.add(site) # a site can belong to multiple tags
                current_app.logger.info('Starting requests for site (tag {}, scan job {})'.
                        format(site.site_name, tag.name, scan_job.id))
                if site.site_type == 'web':
                    search_pointer = site.search_url_for(phrase_tokens)
                    req = ScrapeRequest(target=search_pointer, is_search=True, job_id=scan_job.id,
                            status='waiting',
                            source_type=site.source_type, site_name=site.site_name,
                            site_url=site.homepage_url, site_type=site.site_type,
                            site_id = site.id,
                            query_tags=scan_job.query_tags, save_copies=scan_job.save_copies)
                    website_count += 1
                elif site.site_type == 'reddit':
                    req = ScrapeRequest(target='[reddit] '+scan_job.query_phrase,
                            is_search=True, job_id=scan_job.id,
                            status='waiting', site_type=site.site_type,
                            source_type=site.source_type, site_name=site.site_name,
                            site_url=site.homepage_url,
                            site_id = site.id,
                            query_tags=scan_job.query_tags, save_copies=scan_job.save_copies)
                    subreddit_count += 1
                else:
                    current_app.logger.error('Unknown site type {} for site {} ({})'.format(
                        site.site_type, site.id, site.homepage_url))
                db.session.add(req)
                db.session.commit()
    scan_job.change_status('working')
    scan_job.website_count = website_count
    scan_job.subreddit_count = subreddit_count
    db.session.add(scan_job)
    db.session.commit()
    # TODO failure

#
# NOTE the following also get the job ids, as they are meant to be called by a client function which
# may have an outdated version of the object.
#

def terminate_scan(scan_job_id):
    """
    Terminate the scan job. All waiting scrape requests are cancelled; scheduled ones will still be
    made unless we restart Scrapy.
    """
    scan_job = ScanJob.query.get(scan_job_id)
    waiting_requests = [req for req in scan_job.requests if req.status == 'waiting']
    for req in waiting_requests:
        req.change_status('cancelled')
        db.session.commit()
    scan_job.change_status('terminated')
    db.session.commit()

def scan_progress_info(scan_job_id):
    """
    A dictionary: 'phase' ('waiting', 'search', 'crawl', 'unexisting' or appropriate ScanJob
    status), possibly also 'fails', 'last_url', 'dl_proportion'. Note that dl_proportion reflects
    proportion of the pages to be crawled or the search pages depending on the phase.
    """
    scan_job = ScanJob.query.get(scan_job_id)
    if scan_job is None:
        current_app.logger.debug('Returning status unexisting for the job'.format(scan_job_id))
        return {'phase': 'unexisting'}
    if scan_job.status in ['waiting', 'finished', 'terminated', 'rejected']:
        current_app.logger.debug('Returning status {} for the job'.format(scan_job.status,
            scan_job_id))
        return {'phase': scan_job.status}

    # Otherwise, we try do determine "crawl" or "search" and the detailed progress info.

    # TODO possibly optimize queries
    # Count the committed search and crawl (non-search) requests.
    committed_reddit_search_count = ScrapeRequest.query.filter_by(
            status='committed', site_type='reddit', is_search=True,
            job_id=scan_job.id).count()
    committed_web_search_count = ScrapeRequest.query.filter_by(
            status='committed', site_type='web', is_search=True,
            job_id=scan_job.id).count()
    committed_crawl_count = ScrapeRequest.query.filter_by(
        status='committed', is_search=False, job_id=scan_job.id).count()
    # Count search requests that ran (may not finished, but we have real data for them).
    ran_reddit_search_count = ScrapeRequest.query.filter_by(
            status='ran', site_type='reddit', is_search=True,
            job_id=scan_job.id).count()
    ran_web_search_count = ScrapeRequest.query.filter_by(
            status='ran', site_type='web', is_search=True,
            job_id=scan_job.id).count()
    # Committed Reddit crawls. NOTE that once we implement search paging with reporting the number
    # of pages, we will switch to an analogous method of estimation for Web.
    committed_reddit_crawl_count = ScrapeRequest.query.filter_by(is_search=False,
            status='committed', site_type='reddit', job_id=scan_job.id).count()
    committed_web_crawl_count = ScrapeRequest.query.filter_by(is_search=False,
            status='committed', site_type='web', job_id=scan_job.id).count()
    # NOTE this assumes that the Reddit scraper is single-threaded and processes requests in
    # sequence.
    current_reddit_search = list(ScrapeRequest.query.filter_by(
                    status='ran', site_type='reddit', is_search=True,
                    job_id=scan_job.id))
    reddit_current_load_from_search = (0 if len(current_reddit_search) == 0
            else current_reddit_search[0].lead_count)
    reddit_past_load_from_search = ScrapeRequest.query.with_entities(
             func.sum(ScrapeRequest.lead_count).label("comments_sum")
             ).filter(ScrapeRequest.status=='committed',
                     ScrapeRequest.site_type=='reddit',
                     ScrapeRequest.is_search==True,
                     ScrapeRequest.job_id==scan_job.id).scalar()
    reddit_past_load_from_search = (int(reddit_past_load_from_search)
            if reddit_past_load_from_search is not None
            else 0)
    # Count all search and crawl (non-search) requests.
    full_search_count = ScrapeRequest.query.filter_by(is_search=True, job_id=scan_job.id).count()
    full_web_crawl_count = ScrapeRequest.query.filter_by(is_search=False, site_type='web',
            job_id=scan_job.id).count()
    # Count failures.
    fails_count = ScrapeRequest.query.filter_by(status='failed', job_id=scan_job.id).count()
    # Compute the denominator for computing scan progress. In the estimation, use the real numbers
    # for done search requests and the estimators from config for the future ones.
    reddit_full_estimation = (
            # Estimation for future search requests.
            ((scan_job.subreddit_count - ran_reddit_search_count)
            * int(LiveConfigValue.query.get('subreddit_estimation_multiplier').value))
            # Current load from search.
            + reddit_current_load_from_search
            # Committed requests.
            + reddit_past_load_from_search
            )
    web_full_estimation = (
            ((scan_job.website_count - ran_web_search_count)
            * int(LiveConfigValue.query.get('website_estimation_multiplier').value))
            + committed_web_crawl_count
            )
    full_estimation = reddit_full_estimation + web_full_estimation
    if full_estimation == 0: # avoid / 0 should it happen for whatever reason
        full_estimation = 1

    current_app.logger.debug('Progress estimation for {}:\n'
            'Done subs Reddit: {}, future estimation: {}, current load to crawl {}, '
            'full Reddit estimation: {}, already committed: {}\n'
            'Committed Web: {}, future estimation: {}, full web estimation {}\n'
            'Full estimation numerator is {}, denominator {}'
            .format(scan_job.id,
                # Reddit done
                reddit_past_load_from_search,
                # Reddit future
                (scan_job.subreddit_count - ran_reddit_search_count)
                * int(LiveConfigValue.query.get(
                    'subreddit_estimation_multiplier').value),
                # Reddit current
                reddit_current_load_from_search,
                # Reddit estimation,
                reddit_full_estimation,
                # Reddit committed
                committed_reddit_crawl_count+committed_reddit_search_count,
                # Web done
                committed_web_crawl_count+committed_web_search_count,
                # Web future
                (scan_job.website_count - ran_web_search_count)
                * int(LiveConfigValue.query.get('website_estimation_multiplier').value),
                # Web full estimation
                web_full_estimation,
                # Full estimation components
                (committed_reddit_search_count + committed_web_search_count
                    + committed_crawl_count),
                full_estimation))

    dl_proportion = (committed_reddit_search_count
            + committed_web_search_count
            + committed_crawl_count) / full_estimation
    phase = ('search'
            if (ran_reddit_search_count+ran_web_search_count) < full_search_count
            else 'crawl')
    done_request = list(ScrapeRequest.query.filter(
        (ScrapeRequest.status == 'committed')
        | (ScrapeRequest.status == 'failed'),
        ScrapeRequest.job_id == scan_job.id
        ).order_by(ScrapeRequest.status_changed.desc()).limit(1))
    return {'phase': phase, 'fails': fails_count,
            # TODO test that it's the target, not ScrapeRequest object
            'last_url': done_request[0].target if len(done_request) > 0 else None,
            'dl_proportion': dl_proportion}
