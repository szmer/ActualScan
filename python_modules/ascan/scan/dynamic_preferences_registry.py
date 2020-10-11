from dynamic_preferences.preferences import Section
from dynamic_preferences.registries import global_preferences_registry
from dynamic_preferences.types import IntegerPreference, FloatPreference, StringPreference

#
# Settings related to the scanning process.
#
scanning = Section('scanning', 'Scanning')
@global_preferences_registry.register
class ScanJobTimeToLive(IntegerPreference):
    help_text = """Time (in seconds) before it's auto-terminated."""
    section = scanning
    name = 'scan_job_time_to_live'
    default = 60*60*24
    required = True

@global_preferences_registry.register
class ScanJobTimeToExist(IntegerPreference):
    help_text = """Time (in seconds) before it's deleted from db."""
    section = scanning
    name = 'scan_job_time_to_exist'
    default = 60*60*24*30
    required = True

@global_preferences_registry.register
class ScrapeRequestTimeToExist(IntegerPreference):
    help_text = """Time (in seconds) before it's deleted from db."""
    section = scanning
    name = 'scrape_request_time_to_exist'
    default = 60*60*24*30
    required = True
    
@global_preferences_registry.register
class ConcurrentJobsAllowed(IntegerPreference):
    help_text = """How many scan jobs can run simultaneously"""
    section = scanning
    name = 'concurrent_jobs_allowed'
    default = 20
    required = True

@global_preferences_registry.register
class GuestsScanPermissionsThreshold(IntegerPreference):
    help_text = """How many potential jobs have to be idle to start to issue guest permissions"""
    section = scanning
    name = 'guest_scan_permissions_threshold'
    default = 10
    required = True

@global_preferences_registry.register
class GuestsScanPermissionTimeToLive(IntegerPreference):
    help_text = """Time (in seconds) before it's deleted from db."""
    section = scanning
    name = 'guest_scan_permission_time_to_live'
    default = 60 # a minute
    required = True

@global_preferences_registry.register
class RedditSearchDepth(IntegerPreference):
    help_text = """How many submission to download."""
    section = scanning
    name = 'reddit_search_depth'
    default = 30
    required = True

@global_preferences_registry.register
class RedditManyCommentsThreshold(IntegerPreference):
    help_text = """
    How many comments a submissions has to have when Reddit scraper starts pruning
    low scores
    """
    section = scanning
    name = 'reddit_many_comments_threshold'
    default = 200
    required = True

@global_preferences_registry.register
class RedditManyCommentsMinScoreRatio(IntegerPreference):
    help_text = """
    When there are many comments, the minimal score for inclusion is the comments count divided by
    this much
    """
    section = scanning
    name = 'reddit_many_comments_minscore_ratio'
    default = 200
    required = True

@global_preferences_registry.register
class SeleniumWaitTime(IntegerPreference):
    help_text = """How long to wait for the desired changes on the site when using Selenium"""
    section = scanning
    name = 'selenium_wait_time'
    default = 4
    required = True

@global_preferences_registry.register
class SeleniumMaxClickElemsTried(IntegerPreference):
    help_text = """How many elements max. to try when looking for a next page link in Selenium"""
    section = scanning
    name = 'selenium_max_click_elems_tried'
    default = 3
    required = True

@global_preferences_registry.register
class WebNormalSearchDepth(IntegerPreference):
    help_text = """
    How many pages of search to process per site in normal circumstances (no JS required).
    """
    section = scanning
    name = 'web_normal_search_depth'
    default = 30
    required = True

@global_preferences_registry.register
class WebJSSearchDepth(IntegerPreference):
    help_text = """
    How many pages of search to process when using Selenium to find JS next page links.
    """
    section = scanning
    name = 'web_js_search_depth'
    default = 20
    required = True

@global_preferences_registry.register
class WebInfiscrollSearchDepth(IntegerPreference):
    help_text = """
    How many page-downs of search to process when using Selenium for infinite scroll.
    """
    section = scanning
    name = 'web_infiscroll_search_depth'
    default = 10
    required = True

@global_preferences_registry.register
class DedupDatePostCheck(StringPreference):
    help_text = """
    These are the conditions for URL duplicates from Solr to be actually considered dupes and skipped.
    They function both as OR, not AND.
    """
    section = scanning
    name = 'dedup_date_post_check'
    default = '[* TO NOW-1YEARS]'
    required = True

@global_preferences_registry.register
class DedupDateRetrCheck(StringPreference):
    help_text = """
    These are the conditions for URL duplicates from Solr to be actually considered dupes and skipped.
    They function both as OR, not AND.
    """
    section = scanning
    name = 'dedup_date_retr_check'
    default = '[NOW-12HOURS TO NOW]'
    required = True

#
# Values for progress estimation.
#
estimations = Section('estimations', 'Estimations of scan progress') 

@global_preferences_registry.register
class SubredditEstimationMultiplier(IntegerPreference):
    help_text = """Used to estimate how many scrape requests will a subreddit generate"""
    section = estimations
    name = 'subreddit_estimation_multiplier'
    default = 75 * 100
    required = True

@global_preferences_registry.register
class WebsiteEstimationMultiplier(IntegerPreference):
    help_text = """Used to estimate how many scrape requests will a site generate"""
    section = estimations
    name = 'website_estimation_multiplier'
    default = 50
    required = True

@global_preferences_registry.register
class SearchPageYieldEstimation(IntegerPreference):
    help_text = """How many pages to crawl we expect from a search on a site."""
    section = estimations
    name = 'search_page_yield_estimation'
    default = 10
    required = True

#
# Trust levels.
#
trust_levels = Section('trust_levels', 'Trust levels for tag-site links') 

@global_preferences_registry.register
class TrustLevelThresholdBase(IntegerPreference):
    help_text = """Minimal level for a site to be considered base level"""
    section = trust_levels 
    name = 'trust_level_threshold_base'
    default = 1000
    required = True

@global_preferences_registry.register
class TrustLevelThresholdRespected(IntegerPreference):
    help_text = """Minimal level for a site to be considered respected level"""
    section = trust_levels 
    name = 'trust_level_threshold_respected'
    default = 50
    required = True

@global_preferences_registry.register
class TrustLevelThresholdCommunity(IntegerPreference):
    help_text = """Minimal level for a site to be considered community level"""
    section = trust_levels 
    name = 'trust_level_threshold_community'
    default = 0
    required = True

@global_preferences_registry.register
class FeedbackPermissionTimeToExist(IntegerPreference):
    help_text = """How long should it exist in the DB"""
    section = trust_levels 
    name = 'feedback_permission_time_to_exist'
    default = 60*60*24*30 # a month (in seconds) before it's deleted from db
    required = True

@global_preferences_registry.register
class SiteFeedbackCountPerUser(IntegerPreference):
    help_text = """
    How many times a user can vote on a site (the existing permissions from DB are considered)
    """
    section = trust_levels 
    name = 'site_feedback_count_per_user'
    default = 2
    required = True

@global_preferences_registry.register
class LinkFeedbackCountGlobally(IntegerPreference):
    help_text = """
    How many times we can get feedback for a site-tag connection globally. This should affect IPs and
    the users with non-established trust.
    """
    section = trust_levels 
    name = 'link_feedback_count_globally'
    default = 300
    required = True

@global_preferences_registry.register
class FeedbackAskFrequency(FloatPreference):
    help_text = """
    For what proportion of index queries should we try to give an option of feedback.
    """
    section = trust_levels 
    name = 'feedback_ask_frequency'
    default = 0.5
    required = True

#
# Index searching.
#
index_searching = Section('index_searching', 'Searching the index') 

@global_preferences_registry.register
class DefaultScanTimedeltaStart(IntegerPreference):
    help_text = """How far back in time (in weeks) should the default index search reach."""
    section = index_searching 
    name = 'default_scan_timedelta_start'
    default = 52*10 # in weeks
    required = True

@global_preferences_registry.register
class TopTermsWithAutocompletePhrases(IntegerPreference):
    help_text = """
    For how many terms from the index to get phrase information from the index (this works in the
    background)
    """
    section = index_searching 
    name = 'top_terms_with_autocomplete_phrases'
    default = 5000
    required = True

@global_preferences_registry.register
class OmnivoreConcurrentJobs(IntegerPreference):
    help_text = """How many concurrent Omnivore jobs are allowed to run"""
    section = index_searching 
    name = 'omnivore_concurrent_jobs'
    default = 3
    required = True

@global_preferences_registry.register
class OmnivoreTimeout(IntegerPreference):
    help_text = """After how long to give up on an Omnivore query"""
    section = index_searching 
    name = 'omnivore_timeout'
    default = 60
    required = True

#
# User permissions.
#
user_permissions = Section('user_permissions', 'User permissions')

@global_preferences_registry.register
class MaxTagsPerDay(IntegerPreference):
    help_text = """How many tags daily at most a user can add."""
    section = user_permissions
    name = 'max_tags_per_day'
    default = 15
    required = True

@global_preferences_registry.register
class MaxSitesPerDay(IntegerPreference):
    help_text = """How many sites daily at most a user can add."""
    section = user_permissions
    name = 'max_sites_per_day'
    default = 100
    required = True
