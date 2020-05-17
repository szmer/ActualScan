from flask import current_app
from sqlalchemy import func

from searchfront.extensions import db

from searchfront.blueprints.site import Tag
from searchfront.blueprints.scan_schedule import ScanJob, ScrapeRequest, ScanPermission
from searchfront.blueprints.live_config import LiveConfigValue

def spare_scan_capacity():
    """
    Return the number of additional scan jobs we can accept right now.
    """
    concurrent_jobs_allowed = int(LiveConfigValue.query.get('concurrent_jobs_allowed').value)
    spare_capacity = concurrent_jobs_allowed - ScanJob.query.filter(ScanJob.status.in_(
        ['waiting', 'working'])).count()
    return spare_capacity

def maybe_issue_guest_scan_permission(ip_address):
    """
    Try to issue a guest scan permission for the ip_address, return a boolean indicating success
    or failure.
    """
    guest_scan_permissions_threshold = int(
                LiveConfigValue.query.get('guest_scan_permissions_threshold').value)
    spare_capacity = spare_scan_capacity()
    if spare_capacity >= guest_scan_permissions_threshold:
        permission = ScanPermission(user_ip=ip_address)
        db.session.add(permission)
        db.session.commit()
        return True
    return False

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

    # TODO possibly optimize queries
    # Count the committed search and crawl (non-search) requests.
    # These form the numerator of the "already done" proportion.
    committed_reddit_search_count = ScrapeRequest.query.filter_by(
            status='committed', site_type='reddit', is_search=True,
            job_id=scan_job.id).count()
    committed_reddit_crawl_count = ScrapeRequest.query.filter_by(is_search=False,
            status='committed', site_type='reddit', job_id=scan_job.id).count()
    committed_web_search_count = ScrapeRequest.query.filter_by(
            status='committed', site_type='web', is_search=True,
            job_id=scan_job.id).count()
    committed_crawl_count = ScrapeRequest.query.filter_by(
        status='committed', is_search=False, job_id=scan_job.id).count()

    # Denominator "past" component.
    # Past Reddit load from search is the sum of lead counts (i.e. sums of comments) of the
    # committed Reddit search requests.
    reddit_past_load_from_search = ScrapeRequest.query.with_entities(
             func.sum(ScrapeRequest.lead_count).label("comments_sum")
             ).filter_by(status='committed',
                     site_type='reddit',
                     is_search=True,
                     job_id=scan_job.id).scalar()
    reddit_past_load_from_search = (int(reddit_past_load_from_search)
            if reddit_past_load_from_search is not None
            else 0)
    # What we already know will/has happened with Web pages.
    web_known_yield_from_search = ScrapeRequest.query.filter_by(is_search=False, site_type='web',
            job_id=scan_job.id).count()
    web_search_count = ScrapeRequest.query.filter_by(is_search=True, site_type='web',
            job_id=scan_job.id).count()

    # Denominator "current" component.
    # NOTE this assumes that the Reddit scraper is single-threaded and processes requests in
    # sequence.
    current_reddit_search = list(ScrapeRequest.query.filter_by(
                    status='ran', site_type='reddit', is_search=True,
                    job_id=scan_job.id))
    reddit_current_load_from_search = (0 if len(current_reddit_search) == 0
            else current_reddit_search[0].lead_count)
    # The average number of scraped pages produced by each search page.
    web_committed_crawl_count = ScrapeRequest.query.filter_by(is_search=False, site_type='web',
            job_id=scan_job.id, status='committed').count()
    if web_committed_crawl_count > 0:
        web_committed_search_count = ScrapeRequest.query.filter_by(is_search=True, site_type='web',
            job_id=scan_job.id, status='committed').count()
        average_search_page_yield = web_committed_search_count / web_committed_crawl_count
    else:
        average_search_page_yield = int(
                LiveConfigValue.query.get('search_page_yield_estimation').value)
    # Unfinished search pages, which we will estimate with average yield from above.
    unfinished_search_pages_sum = ScrapeRequest.query.filter(
            ScrapeRequest.is_search==True,
            ScrapeRequest.job_id==scan_job.id,
            ScrapeRequest.status.in_(['waiting', 'scheduled', 'ran'])).count()
    # Conservatively assume that each scheduled search page will yield 2 * more additional ones.
    web_current_yield_from_search = 3 * unfinished_search_pages_sum * average_search_page_yield

    # The denominator "future" component (values we'll use together with subreddit/site counts from
    # the job object.)
    # Count search requests that ran (may not finished in the case of Reddit, but we have real data
    # for them).
    ran_reddit_search_count = ScrapeRequest.query.filter_by(
            status='ran', site_type='reddit', is_search=True,
            job_id=scan_job.id).count()
    # See search for how many sites already fully committed.
    unique_web_search_count = db.session.query(ScrapeRequest.site_name).filter_by(
            site_type='web',
            is_search=True,
            job_id=scan_job.id,
            status='committed').distinct().count()

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
            # Estimation for future search requests.
            ((scan_job.website_count - unique_web_search_count)
            * int(LiveConfigValue.query.get('website_estimation_multiplier').value))
            # Current load from search.
            + web_current_yield_from_search
            # Known yield from search - requests from the search sites that were already scheduled,
            # run or committed (or any status, but known by url).
            + web_known_yield_from_search
            + web_search_count
            )
    full_estimation = reddit_full_estimation + web_full_estimation
    if full_estimation == 0: # avoid / 0 should it happen for whatever reason
        full_estimation = 1

    current_app.logger.debug('Progress estimation for {}:\n'
            'Done subs Reddit: {}, future estimation: {}, current load to crawl {}, '
            'full Reddit estimation: {}, already committed: {}\n'
            'Known yield in Web: {} (+search pages {}), future estimation: {}, estimation of '
            'current yield {}, full web estimation {}\n'
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
                # Web known yield
                web_known_yield_from_search,
                web_search_count,
                # Web future
                (scan_job.website_count - unique_web_search_count)
                * int(LiveConfigValue.query.get('website_estimation_multiplier').value),
                # Web estimation of the current yield
                web_current_yield_from_search,
                # Web full estimation
                web_full_estimation,
                # Full estimation components
                (committed_reddit_search_count + committed_web_search_count
                    + committed_crawl_count),
                full_estimation))

    dl_proportion = (committed_reddit_search_count
            + committed_web_search_count
            + committed_crawl_count) / full_estimation
    phase = 'crawl' # as the search isn't prioritzed, see above
    done_request = list(ScrapeRequest.query.filter(
        (ScrapeRequest.status == 'committed')
        | (ScrapeRequest.status == 'failed'),
        ScrapeRequest.job_id == scan_job.id
        ).order_by(ScrapeRequest.status_changed.desc()).limit(1))
    return {'phase': phase, 'fails': fails_count,
            # TODO test that it's the target as string, not ScrapeRequest object
            'last_url': done_request[0].target if len(done_request) > 0 else None,
            'dl_proportion': dl_proportion}
