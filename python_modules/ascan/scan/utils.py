import colorsys
import hashlib
import os

from dynamic_preferences.registries import global_preferences_registry
from redis import Redis

redis = Redis(host='redis', port=6379, db=0, password=os.environ['REDIS_PASS'])

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

def string_color(string):
    """
    Return a HTML RGB string for the argument.
    """
    str_hash = int(hashlib.md5(string.encode('utf-8')).hexdigest(), 16)
    # Use the first three bytes as HLS values.
    h = ((str_hash & (0xff << (1 * 8))) >> (1 * 8)) / 256
    l = (str_hash & ((0xff << (3 * 8))) >> (3 * 8)) / 256
    s = ((str_hash & (0xff << (2 * 8))) >> (2 * 8)) / 256
    # Ensure that the lightness and saturation are within the desired bounds.
    l = min(0.8, max(l, 0.5))
    s = min(0.6, max(s, 0.3))
    rgb_components = colorsys.hls_to_rgb(h, l, s)
    return '#{:0>1X}{:0>1X}{:0>1X}'.format(*[int(c * 256) for c in rgb_components])
