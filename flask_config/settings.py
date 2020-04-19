# Flask
DEBUG = True
TEMPLATES_AUTO_RELOAD = True

# Flask WTF
WTF_CSRF_ENABLED = True

# Flask Debug Toolbar
#DEBUG_TB_ENABLED = False
DEBUG_TB_INTERCEPT_REDIRECTS = False

# Flask-Security
SECURITY_REGISTERABLE = True
SECURITY_CHANGEABLE = True

LIVECONFIG_START_VALUES = {
        'scan_job_time_to_live': 60*60*24*30, # a month (in seconds) before it's deleted from db
        'scrape_request_time_to_live': 60*60*24*30,
        'concurrent_jobs_allowed': 20,

        'reddit_search_depth': 100,
        'reddit_many_comments_threshold': 200,
        # When there are many comments, the minimal score for inclusion is the comments count
        # divided by this much.
        'reddit_many_comments_minscore_ratio': 200,

        # These multipliers are used to estimate how many scrape requests will a site generate.
        'subreddit_estimation_multiplier': 75 * 100, # submissions * comments per submission
        'website_estimation_multiplier': 50,

        'site_level_threshold_base': 1000,
        'site_level_threshold_community': 0,
        'tag_level_threshold_base': 1000,
        'tag_level_threshold_community': 0,
        }
