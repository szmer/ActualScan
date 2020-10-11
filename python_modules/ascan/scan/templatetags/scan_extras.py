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

@register.filter
def GET_query(field):
    if type(field.value()) == list:
        return '&'.join(['{}={}'.format(field.html_name, elem) for elem in field.value()])
    else:
        return '{}={}'.format(field.html_name, field.value())
