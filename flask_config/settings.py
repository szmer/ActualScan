DEBUG = True

LIVECONFIG_START_VALUES = {
        'scan_job_time_to_live': str(60*60*24*30), # a month (in seconds)
        'scrape_request_time_to_live': str(60*60*24*30),
        'concurrent_jobs_allowed': str(20),

        'reddit_search_depth': 100,
        'reddit_many_comments_threshold': 200,
        # when there are many comments, the minimal score for inclusion is the comments count
        # divided by this much
        'reddit_many_comments_minscore_ratio': 200,
        }
