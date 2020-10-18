import os

from dynamic_preferences.registries import global_preferences_registry
from redis import Redis

redis = Redis(host='redis', port=6379, db=0, password=os.environ['REDIS_PASS'])

class OmnivoreError(RuntimeError):
    pass

class OmnivoreBlocked(RuntimeError):
    pass

def date_fmt(time_obj):
    """
    The format liked by Solr.
    """
    return time_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def trust_level_to_numeric(str_level):
    global_preferences = global_preferences_registry.manager()
    return global_preferences['trust_levels__trust_level_threshold_'+str_level]

def numeric_to_trust_level(value):
    global_preferences = global_preferences_registry.manager()
    if value >= global_preferences['trust_levels__trust_level_threshold_base']:
        return 'base'
    if value >= global_preferences['trust_levels__trust_level_threshold_respected']:
        return 'respected'
    if value >= global_preferences['trust_levels__trust_level_threshold_community']:
        return 'community'
