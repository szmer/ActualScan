from datetime import datetime, timedelta, timezone
from logging import debug, info, error

from scan.models import Site, Tag, ScanJob, ScrapeRequest, ScanPermission, FeedbackPermission

from dynamic_preferences.registries import global_preferences_registry

def verify_scan_permission(user, ip, specific_id=False):
    debug('Is user authd in scan verification: {} (IP {})'.format(
        user.is_authenticated, ip))
    # A logged in user.
    if user.is_authenticated:
        # TODO KLUDGE currently unlimited permission issuance for registered
        if not specific_id:
            return True
        else:
            job = ScanJob.objects.get(id=specific_id)
            if job is None:
                return False
            else:
                return job.user_id == user.id
    # A non-logged in user. Check if they have a scan permission issued.
    if not specific_id: # try to use a permission
        scan_permission = ScanPermission.objects.filter(user_ip=ip, is_used=False)
        if len(scan_permission) > 0:
            for perm in scan_permission: # expire the used permissions
                perm.is_used = True
            return True
    else: # try to find the matching job
        job = ScanJob.objects.get(id=specific_id)
        if job is None:
            return False
        else:
            return job.user_ip == ip
    return False

def spare_scan_capacity():
    """
    Return the number of additional scan jobs we can accept right now.
    """
    global_preferences = global_preferences_registry.manager()
    spare_capacity = global_preferences['scanning__concurrent_jobs_allowed'] - ScanJob.objects.filter(
            status__in=['waiting', 'working']).count()
    return spare_capacity

def maybe_issue_guest_scan_permission(ip_address):
    """
    Try to issue a guest scan permission for the ip_address, return a boolean indicating success
    or failure.
    """
    global_preferences = global_preferences_registry.manager()
    spare_capacity = spare_scan_capacity()
    if spare_capacity >= global_preferences['scanning__guest_scan_permissions_threshold']:
        ScanPermission.objects.create(user_ip=ip_address)
        return True
    return False

def maybe_issue_feedback_permission(user_iden, possible_subject_links, is_ip=False):
    global_preferences = global_preferences_registry.manager()
    affected_sites = [link.site for link in possible_subject_links]
    if is_ip:
        # no matter if used and from when (if they're unimportant, should be deleted)
        prev_feedbacks = FeedbackPermission.objects.filter(user_ip=user_iden,
                subject__site__in=affected_sites).all()
    else:
        prev_feedbacks = FeedbackPermission.objects.filter(user=user_iden,
                subject__site__in=affected_sites).all()
    # Collect how many times we got feedback on each site, to exclude ones that got
    # many feedbacks.
    site_feedback_counts = dict()
    for feedback in prev_feedbacks:
        if not feedback.subject.site in site_feedback_counts:
            site_feedback_counts[feedback.subject.site] = 0
        else:
            site_feedback_counts[feedback.subject.site] += 1
    # Go through the possible links and try to find one where feedback should be possible.
    for link in possible_subject_links:
        if (not link.site in site_feedback_counts
                or site_feedback_counts[link.site]
                < global_preferences['trust_levels__site_feedback_count_per_user']):
            # TODO apply this only to IPs and untrusted users
            global_feedback_count = FeedbackPermission.objects.filter(subject=link).count()
            if global_feedback_count < global_preferences['trust_levels__link_feedback_count_globally']:
                if is_ip:
                    FeedbackPermission.objects.create(user_ip=user_iden, subject=link)
                else:
                    FeedbackPermission.objects.create(user=user_iden, subject=link)
                return link
    return False

def request_scan(user_iden, query_phrase : str, start_date, end_date,
        query_tags=[], query_site_names=[], allow_undated=True, minimal_level=0,
        force_new=False, is_ip=False, is_privileged=False):
    """
    Put an awaiting scan job in the database. query_tags should be a list of strings. If force_new
    is set and the job already exists, a ValueError is raised. Return the ScanJob object.

    Note that is_ip flag sets how to treat user_iden parameter (as user obj in the DB, or as an IP
    string).
    """
    # TODO also recreate the job if in an inactive status
    query_tags_str = ','.join(query_tags)
    query_sites_str = ','.join(query_site_names)
    # Try to get already existing jobs with the same parameters
    # NOTE deduplicating them across users
    if is_privileged:
        # Try to find still working jobs.
        jobs = ScanJob.objects.filter(query_phrase=query_phrase, query_tags=query_tags_str,
                query_site_names=query_sites_str, status__in=['waiting', 'working'],
                start_date=start_date, end_date=end_date, allow_undated=allow_undated,
                minimal_level=minimal_level)
        if not jobs:
            # TODO let them choose if the scan should be new
            time_threshold = datetime.now() - timedelta(minutes=5)
            jobs = ScanJob.objects.filter(query_phrase=query_phrase, query_tags=query_tags_str,
                    query_site_names=query_sites_str, status='finished',
                    status_changed__gte=time_threshold,
                    start_date=start_date, end_date=end_date, allow_undated=allow_undated,
                    minimal_level=minimal_level)
    # Non-privileged users can start a scan every 30 minutes. TODO notify them
    else:
        time_threshold = datetime.now() - timedelta(minutes=30)
        jobs = ScanJob.objects.filter(query_phrase=query_phrase, query_tags=query_tags_str,
                query_site_names=query_sites_str, minimal_level=minimal_level,
                start_date=start_date, end_date=end_date, allow_undated=allow_undated,
                status_changed__gte=time_threshold)
    debug('The user is privileged: {}, number of jobs found:'.format(is_privileged,
        0 if not jobs else len(jobs)))
    if jobs and force_new:
        jobs.delete()
        jobs = False
    if not jobs:
        if not is_ip:
            job = ScanJob.objects.create(user=user_iden, query_phrase=query_phrase,
                    start_date=start_date, end_date=end_date, allow_undated=allow_undated,
                    query_tags=query_tags_str, query_site_names=query_sites_str, status='waiting')
        else:
            job = ScanJob.objects.create(user_ip=user_iden, query_phrase=query_phrase,
                    start_date=start_date, end_date=end_date, allow_undated=allow_undated,
                    query_tags=query_tags_str, query_site_names=query_sites_str, status='waiting')
        jobs = [job] # the return statement wants a list
    return jobs[0]

def start_scan(scan_job):
    """
    Start the scan_job (put the scrape requests).
    """
    info('Starting scan job {}'.format(scan_job.id))
    phrase_tokens = scan_job.query_phrase.split()
    sites_to_query = set()
    website_count = 0
    subreddit_count = 0
    if scan_job.query_site_names:
        site_strs = scan_job.query_site_names.split(',')
        info('Scanning the sites: {}'.format(site_strs))
        for site in Site.objects.filter(site_name__in=site_strs):
            sites_to_query.add(site)
    if scan_job.query_tags:
        # TODO test for the situation with no coherent/findable tags
        tag_strs = scan_job.query_tags.split(',')
        info('Tags taken into account in this scan: {}'.format(tag_strs))
        tags = Tag.objects.filter(name__in=tag_strs)
        for tag in tags:
            for site_link in tag.site_links.filter(level__gte=scan_job.minimal_level).all():
                site = site_link.site
                if not site in sites_to_query:
                    sites_to_query.add(site) # a site can belong to multiple tags
    for site in sites_to_query:
        info('Starting requests for site {} (scan job {})'.
                format(site.site_name, scan_job.id))
        if site.site_type == 'web':
            search_pointer = site.search_url_for(phrase_tokens)
            ScrapeRequest.objects.create(target=search_pointer,
                    is_search=True, job=scan_job,
                    status='waiting',
                    source_type=site.source_type, site_name=site.site_name,
                    site_url=site.homepage_url, site_type=site.site_type,
                    site_id = site.id,
                    save_copies=scan_job.save_copies or site.save_copies)
            website_count += 1
        elif site.site_type == 'reddit':
            ScrapeRequest.objects.create(target='[reddit] '+scan_job.query_phrase,
                    is_search=True, job=scan_job,
                    status='waiting', site_type=site.site_type,
                    source_type=site.source_type, site_name=site.site_name,
                    site_url=site.homepage_url,
                    site_id = site.id,
                    save_copies=scan_job.save_copies or site.save_copies)
            subreddit_count += 1
        else:
            error('Unknown site type {} for site {} ({})'.format(
                site.site_type, site.id, site.homepage_url))
    scan_job.change_status('working')
    scan_job.website_count = website_count
    scan_job.subreddit_count = subreddit_count
    scan_job.save()
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
    scan_job = ScanJob.objects.get(id=scan_job_id)
    scan_job.requests.filter(status__in=['waiting', 'scheduled']).update(status='cancelled',
                                            status_changed=datetime.now(timezone.utc))
    scan_job.change_status('terminated')

def scan_progress_info(scan_job_id):
    """
    A dictionary: 'phase' ('waiting', 'working', 'unexisting' or appropriate ScanJob
    status), possibly also 'fails', 'last_url', 'dl_proportion'. Note that dl_proportion reflects
    proportion of the pages to be crawled or the search pages depending on the phase.
    """
    global_preferences = global_preferences_registry.manager()
    scan_job = ScanJob.objects.get(id=scan_job_id)
    if scan_job is None:
        debug('Returning status unexisting for the job'.format(scan_job_id))
        return {'phase': 'unexisting'}

    # TODO possibly optimize queries
    # Count the committed search and crawl (non-search) requests.
    # These form the numerator of the "already done" proportion.
    committed_reddit_search_count = ScrapeRequest.objects.filter(
            status='committed', site_type='reddit', is_search=True,
            job=scan_job).count()
    committed_reddit_crawl_count = ScrapeRequest.objects.filter(is_search=False,
            status='committed', site_type='reddit', job=scan_job).count()
    committed_web_search_count = ScrapeRequest.objects.filter(
            status='committed', site_type='web', is_search=True,
            job=scan_job).count()
    committed_web_crawl_count = ScrapeRequest.objects.filter(is_search=False, site_type='web',
            job=scan_job, status='committed').count()
    committed_crawl_count = ScrapeRequest.objects.filter(
        status='committed', is_search=False, job=scan_job).count()

    # Denominator "past" component - what we already know will/has happened with the pages.
    reddit_known_yield_from_search = ScrapeRequest.objects.filter(is_search=False, site_type='reddit',
            job=scan_job).count()
    web_known_yield_from_search = ScrapeRequest.objects.filter(is_search=False, site_type='web',
            job=scan_job).count()
    web_search_count = ScrapeRequest.objects.filter(is_search=True, site_type='web',
            job=scan_job).count()

    # Denominator "current" component: search request that are currently yielding new stuff to
    # scrape.
    current_reddit_search_count = ScrapeRequest.objects.filter(
                    status='ran', site_type='reddit', is_search=True,
                    job=scan_job).count()
    if committed_reddit_search_count > 0:
        average_reddit_search_yield = committed_reddit_crawl_count / committed_reddit_search_count
    else:
        average_reddit_search_yield = global_preferences['estimations__subreddit_estimation_multiplier']
    reddit_current_yield_from_search = current_reddit_search_count * average_reddit_search_yield
    # The average number of scraped pages produced by each search page.
    if committed_web_search_count > 0:
        average_search_page_yield = committed_web_crawl_count / committed_web_search_count 
    else:
        average_search_page_yield = global_preferences['estimations__search_page_yield_estimation']
    # Unfinished search pages, which we will estimate with average yield from above.
    unfinished_search_pages_sum = ScrapeRequest.objects.filter(
            is_search=True,
            job=scan_job,
            status__in=['waiting', 'scheduled', 'ran']).count()
    unfinished_crawl_pages_sum = ScrapeRequest.objects.filter(
            is_search=False,
            job=scan_job,
            status__in=['waiting', 'scheduled', 'ran']).count()
    # Conservatively assume that each scheduled search page will yield 2 * more additional ones.
    web_current_yield_from_search = 3 * unfinished_search_pages_sum * average_search_page_yield

    # The denominator "future" component (values we'll use together with subreddit/site counts from
    # the job object.)
    # Count search requests that ran (may not finished in the case of Reddit, but we have real data
    # for them).
    waiting_reddit_search_count = ScrapeRequest.objects.filter(
            status='ran', site_type='reddit', is_search=True,
            job=scan_job).count()
    # See search for how many sites already fully committed.
    unique_sites_committed = set(ScrapeRequest.objects.filter(
            job=scan_job,
            status__in=['committed', 'failed']).values_list('site_name', flat=True))
    unique_sites_waiting = set(ScrapeRequest.objects.filter(
            job=scan_job,
            status__in=['scheduled', 'waiting', 'ran']).values_list('site_name', flat=True))
    unique_sites_done = unique_sites_committed.difference(unique_sites_waiting)
    unique_sites_worked = unique_sites_waiting.difference(unique_sites_done)
    unique_web_sites_known_count = len([site for site in unique_sites_worked
        if site[:3] != '/r/'] + [site for site in unique_sites_done if site[:3] != '/r/'])

    # Count failures.
    fails_count = ScrapeRequest.objects.filter(status='failed', job=scan_job).count()

    # Big picture request stats (this goes to the client).
    req_stats = {
            'searches': (committed_reddit_search_count+committed_web_search_count
                +unfinished_search_pages_sum),
            'crawls': (committed_reddit_crawl_count+committed_web_crawl_count
                +unfinished_crawl_pages_sum),
            'commits': (committed_reddit_search_count+committed_reddit_crawl_count
                +committed_web_search_count+committed_web_crawl_count),
            'wait_runs': unfinished_crawl_pages_sum+unfinished_search_pages_sum,
            'fails': fails_count,
            'future': ((scan_job.website_count - unique_web_sites_known_count)
                * global_preferences['estimations__website_estimation_multiplier'] 
                + (scan_job.subreddit_count - waiting_reddit_search_count
                    - committed_reddit_search_count)
                * average_reddit_search_yield),
            'reddit_blocking': (waiting_reddit_search_count and not committed_reddit_crawl_count
                and not reddit_current_yield_from_search)
            }

    # Compute the denominator for computing scan progress. In the estimation, use the real numbers
    # for done search requests and the estimators from config for the future ones.
    reddit_full_estimation = (
            # Estimation for future search requests.
            ((scan_job.subreddit_count - waiting_reddit_search_count - committed_reddit_search_count)
            * average_reddit_search_yield)
            # Current load from search.
            + reddit_current_yield_from_search
            # Committed requests.
            + reddit_known_yield_from_search
            )
    web_full_estimation = (
            # Estimation for future search requests.
            ((scan_job.website_count - unique_web_sites_known_count)
            * global_preferences['estimations__website_estimation_multiplier'])
            # Current load from search.
            + web_current_yield_from_search
            # Known yield from search - requests from the search sites that were already scheduled,
            # run or committed (or any status, but known by url).
            + web_known_yield_from_search
            + web_search_count
            )
    full_estimation = reddit_full_estimation + web_full_estimation

    if scan_job.status in ['waiting', 'finished', 'terminated', 'rejected']:
        phase = scan_job.status
    else:
        phase = 'working'

    if scan_job.status == 'finished':
        dl_proportion = 1.0
    else:
        debug('Progress estimation for {}:\n'
                'Done subs Reddit: {}, future estimation: {}, current load to crawl {}, '
                'full Reddit estimation: {}, already committed: {}\n'
                'Known yield in Web: {} (+search pages {}), future estimation: {}, estimation of '
                'current yield {}, full web estimation {}\n'
                'Full estimation numerator is {}, denominator {}'
                .format(scan_job.id,
                    # Reddit done
                    reddit_known_yield_from_search,
                    # Reddit future
                    (scan_job.subreddit_count - waiting_reddit_search_count
                        - committed_reddit_search_count)
                    * average_reddit_search_yield,
                    # Reddit current
                    reddit_current_yield_from_search,
                    # Reddit estimation,
                    reddit_full_estimation,
                    # Reddit committed
                    committed_reddit_crawl_count+committed_reddit_search_count,
                    # Web known yield
                    web_known_yield_from_search,
                    web_search_count,
                    # Web future
                    (scan_job.website_count - unique_web_sites_known_count)
                    * global_preferences['estimations__website_estimation_multiplier'],
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
                + committed_crawl_count)
        if full_estimation > 0:
            dl_proportion /= full_estimation
        else:
            dl_proportion = 0.0
    done_requests = ScrapeRequest.objects.filter(
            status__in=['committed', 'failed'], job=scan_job).exclude(
                    site_type='reddit', is_search=True).order_by('-status_changed')[:3]
    return {'phase': phase, 'fails': fails_count,
            'last_urls': [{'time': req.status_changed.strftime('%b %d %Y, %H:%I %Z'),
                'url': req.target, 'site': req.site_name } for req in done_requests],
            'req_stats': req_stats, 'sites_done': list(unique_sites_done),
            'sites_worked': list(unique_sites_worked),
            'dl_proportion': dl_proportion}
