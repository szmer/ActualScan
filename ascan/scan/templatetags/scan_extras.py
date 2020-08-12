from django import template
from dynamic_preferences.registries import global_preferences_registry

register = template.Library()

@register.filter
def format_trust_level(value):
    global_preferences = global_preferences_registry.manager()
    if value >= global_preferences['trust_level_threshold_base']:
        return 'base'
    if value >= global_preferences['trust_level_threshold_respected']:
        return 'respected'
    if value >= global_preferences['trust_level_threshold_community']:
        return 'community'
