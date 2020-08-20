from dynamic_preferences.types import IntegerPreference, FloatPreference
from dynamic_preferences.registries import global_preferences_registry

@global_preferences_registry.register
class ScanJobTimeToLive(IntegerPreference):
    name = 'scan_job_time_to_live'
    default = 60*60*24 # a day (in seconds) before it's auto-terminated
    required = True

@global_preferences_registry.register
class ScanJobTimeToExist(IntegerPreference):
    name = 'scan_job_time_to_exist'
    default = 60*60*24*30 # a month (in seconds) before it's deleted from db
    required = True

@global_preferences_registry.register
class ScrapeRequestTimeToExist(IntegerPreference):
    name = 'scrape_request_time_to_exist'
    default = 60*60*24*30 # a month (in seconds) before it's deleted from db
    required = True
#
# Scan permission issuance.
#
# how many scan jobs can run simultaneously
@global_preferences_registry.register
class ConcurrentJobsAllowed(IntegerPreference):
    name = 'concurrent_jobs_allowed'
    default = 20
    required = True

# how many potential jobs have to be idle to start to issue guest permissions
@global_preferences_registry.register
class GuestsScanPermissionsThreshold(IntegerPreference):
    name = 'guest_scan_permissions_threshold'
    default = 10
    required = True

@global_preferences_registry.register
class GuestsScanPermissionTimeToLive(IntegerPreference):
    name = 'guest_scan_permission_time_to_live'
    default = 60 # a minute
    required = True

#
# Reddit scraper configuration.
#
@global_preferences_registry.register
class RedditSearchDepth(IntegerPreference):
    name = 'reddit_search_depth'
    default = 100 # 100 submissions
    required = True

@global_preferences_registry.register
class RedditManyCommentsThreshold(IntegerPreference):
    name = 'reddit_many_comments_threshold'
    default = 200
    required = True

# When there are many comments, the minimal score for inclusion is the comments count
# divided by this much.
@global_preferences_registry.register
class RedditManyCommentsMinScoreRatio(IntegerPreference):
    name = 'reddit_many_comments_minscore_ratio'
    default = 200
    required = True

#
# Web scraper configuration (for scrapy).
#
@global_preferences_registry.register
class SeleniumWaitTime(IntegerPreference):
    name = 'selenium_wait_time'
    default = 4
    required = True

@global_preferences_registry.register
class SeleniumMaxClickElemsTried(IntegerPreference):
    name = 'selenium_max_click_elems_tried'
    default = 3
    required = True

@global_preferences_registry.register # NOTE not implemented
class WebNormalSearchDepth(IntegerPreference):
    name = 'web_normal_search_depth'
    default = 40
    required = True

@global_preferences_registry.register
class WebJSSearchDepth(IntegerPreference):
    name = 'web_js_search_depth'
    default = 20
    required = True

@global_preferences_registry.register
class WebInfiscrollSearchDepth(IntegerPreference):
    name = 'web_infiscroll_search_depth'
    default = 10
    required = True

#
# For progress estimations.
#
# These multipliers are used to estimate how many scrape requests will a site generate.
@global_preferences_registry.register
class SubredditEstimationMultiplier(IntegerPreference):
    name = 'subreddit_estimation_multiplier'
    default = 75 * 100 # submissions * comments per submission
    required = True
@global_preferences_registry.register
class WebsiteEstimationMultiplier(IntegerPreference):
    name = 'website_estimation_multiplier'
    default = 50
    required = True

# How many pages to crawl we expect from a random search page.
@global_preferences_registry.register
class SearchPageYieldEstimation(IntegerPreference):
    name = 'search_page_yield_estimation'
    default = 10
    required = True

# thresholds translating numerical site/tag link levels to descriptive levels
@global_preferences_registry.register
class TrustLevelThresholdBase(IntegerPreference):
    name = 'trust_level_threshold_base'
    default = 1000
    required = True
@global_preferences_registry.register
class TrustLevelThresholdRespected(IntegerPreference):
    name = 'trust_level_threshold_respected'
    default = 50
    required = True
@global_preferences_registry.register
class TrustLevelThresholdCommunity(IntegerPreference):
    name = 'trust_level_threshold_community'
    default = 0
    required = True

# How far back in time should the default scan/index search reach.
@global_preferences_registry.register
class DefaultScanTimedeltaStart(IntegerPreference):
    name = 'default_scan_timedelta_start'
    default = 52*10 # in weeks
    required = True

@global_preferences_registry.register
class FeedbackPermissionTimeToExist(IntegerPreference):
    name = 'feedback_permission_time_to_exist'
    default = 60*60*24*30 # a month (in seconds) before it's deleted from db
    required = True

# How many times a user can vote on a site (existing permissions from DB are accounted)
@global_preferences_registry.register
class SiteFeedbackCountPerUser(IntegerPreference):
    name = 'site_feedback_count_per_user'
    default = 2
    required = True

# How many times we can get feedback for a site-tag connection globally. This should affect IPs and
# the users with non-established trust.
@global_preferences_registry.register
class LinkFeedbackCountGlobally(IntegerPreference):
    name = 'link_feedback_count_globally'
    default = 300
    required = True

# For what proportion of index queries should we try to give an option of feedback.
@global_preferences_registry.register
class FeedbackAskFrequency(FloatPreference):
    name = 'feedback_ask_frequency'
    default = 0.5
    required = True
