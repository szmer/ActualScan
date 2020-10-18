import http.client
import json
import numbers
import re

from omnivore2_conf import (
        MINIMUM_CONTEXT_SIZE, MAXIMUM_CONTEXT_SIZE, SOLR_HOST, SOLR_PORT, SOLR_CORE
        )

def solr_query_field_for_period(period_dict):
    """
    Given a period dict, create a string for Solr (main) query for context for this text period.
    """
    return 'q=tags:' + period_dict['tags']

def solr_boost_field_for_period(period_dict):
    """
    Given a period dict, create a string of a JSON Solr bf (boost function) query for context for 
    this text period.
    """
    field_parts = []
    for key, value in period_dict.items():
        if not key in ['date_retr', 'date_class']:
            if isinstance(value, numbers.Number):
                # there is a problem of diffrent ranges/scales which we ignore for now
                field_parts.append('recip(abs({}-({})),1,1000,1000)'.format(key, value))
            elif (isinstance(value, str)
                    and re.search("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", value)):
                # For times, get the difference in ms and use a bigger scale.
                field_parts.append('recip(abs(ms({}, {})),1,100000000,100000000)'.format(key, value))
    return 'bf=' + ' '.join(field_parts)

def context_for_period(period_dict, minimum_size=MINIMUM_CONTEXT_SIZE,
        maximum_size=MAXIMUM_CONTEXT_SIZE):
    solr_address = '/solr/{}/select'.format(SOLR_CORE)
    solr_conn = http.client.HTTPConnection(SOLR_HOST, port=SOLR_PORT, timeout=15)
    solr_conn.request('GET', solr_address)
    response = solr_conn.getresponse()
    response_json = json.loads(response.read().decode('utf-8'))
    return response_json['response']['docs']
