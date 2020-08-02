from django import template
from dynamic_preferences.registries import global_preferences_registry

register = template.Library()

@register.filter
def format_site_level(value):
    global_preferences = global_preferences_registry.manager()
    if value >= global_preferences['site_level_threshold_base']:
        return 'base'
    if value >= global_preferences['site_level_threshold_respected']:
        return 'respected'
    if value >= global_preferences['site_level_threshold_community']:
        return 'community'
    return 'spam'

@register.filter
def format_tag_level(value):
    global_preferences = global_preferences_registry.manager()
    if value >= global_preferences['tag_level_threshold_base']:
        return 'base'
    if value >= global_preferences['tag_level_threshold_respected']:
        return 'respected'
    if value >= global_preferences['tag_level_threshold_community']:
        return 'community'
    return 'spam'
