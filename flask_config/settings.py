DEBUG = True

LIVECONFIG_START_VALUES = {
        'scan_job_time_to_live': str(60*60*24*30), # a month (in seconds)
        'scrape_request_time_to_live': str(60*60*24*30),
        'concurrent_jobs_allowrd': str(20)
        }
