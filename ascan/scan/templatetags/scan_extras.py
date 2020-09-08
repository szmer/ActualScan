from urllib.parse import quote

from django import template

from scan.utils import numeric_to_trust_level

register = template.Library()

@register.filter
def format_trust_level(value):
    return numeric_to_trust_level(value)

@register.filter
def full_escape(value):
    return quote(repr(value).replace('\'', ''), safe='')
