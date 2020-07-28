from dynamic_preferences.types import IntegerPreference
from dynamic_preferences.registries import global_preferences_registry

# we create some section objects to link related preferences together

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

# Scan permission issuance.

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

# how many potential jobs have to be idle to start to issue guest permissions
@global_preferences_registry.register
class GuestsScanPermissionTimeToLive(IntegerPreference):
    name = 'guest_scan_permission_time_to_live'
    default = 60 # a minute
    required = True

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

# thresholds translating numerical site/tag levels to descriptive levels
@global_preferences_registry.register
class SiteLevelThresholdBase(IntegerPreference):
    name = 'site_level_threshold_base'
    default = 1000
    required = True
@global_preferences_registry.register
class SiteLevelThresholdCommunity(IntegerPreference):
    name = 'site_level_threshold_community'
    default = 0
    required = True
@global_preferences_registry.register
class TagLevelThresholdBase(IntegerPreference):
    name = 'tag_level_threshold_base'
    default = 1000
    required = True
@global_preferences_registry.register
class TagLevelThresholdCommunity(IntegerPreference):
    name = 'tag_level_threshold_community'
    default = 0
    required = True
