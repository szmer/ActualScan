from datetime import datetime 
import http.client
import json
from logging import debug, info
from urllib import parse
import os

from django.conf import settings
from redis import Redis

from scan.utils import date_fmt

redis = Redis(host='redis', port=6379, db=0, password=os.environ['REDIS_PASS'])

def rules_results(scan_query, rules, query_site_names=''):
    context = dict()
    context['rules_string'] = rules
    context['rules'] = {}
    solr_filter_terms = []
    solr_boost_terms = []
    if rules:
        for rule in rules.split(';'):
            parts = rule.split(',')
            field_name, min_term, max_term, boost_term = (
                    settings.SOLR_FEATURE_CODES[parts[0]], parts[1], parts[2], parts[3]
                    )
            if 'date' in field_name:
                solr_filter_terms.append('{}: [{} TO {}]'.format(field_name,
                    date_fmt(datetime.strptime(min_term, '%m/%d/%Y')) if min_term != '*' else '*',
                    date_fmt(datetime.strptime(max_term, '%m/%d/%Y')) if max_term != '*' else '*'))
            elif field_name.endswith('_i'):
                solr_filter_terms.append('{}: [{} TO {}]'.format(field_name,
                    int(float(min_term)) if min_term != '*' else '*',
                    int(float(max_term)) if max_term != '*' else '*'))
            else:
                solr_filter_terms.append('{}: [{} TO {}]'.format(field_name, min_term, max_term))
            solr_boost_terms.append('sum(0.001,mul({}, {}))'.format(field_name,  boost_term))
            context['rules'][field_name] = { 'min': min_term, 'max': max_term,
                    'weight': boost_term }

    solr_address = '/solr/{}/select'.format(settings.SOLR_CORE)
    solr_address += '?defType=edismax&q={}&qf={}'.format(parse.quote(scan_query, safe=''),
            parse.quote('text_en text_xx', safe=''))
    if query_site_names:
        solr_address += '&' + parse.quote(' '.join(query_site_names), safe='')
    for filter_term in solr_filter_terms:
        solr_address += '&fq=' + parse.quote(filter_term, safe='')
    for boost_term in solr_boost_terms:
        solr_address += '&boost=' + parse.quote(boost_term, safe='')
    solr_address += '&stats=true'
    for code, field_name in settings.SOLR_FEATURE_CODES.items():
        solr_address += '&stats.field='+field_name
    info('Constructed Solr query {}'.format(solr_address))
    solr_conn = http.client.HTTPConnection(settings.SOLR_HOST, port=settings.SOLR_PORT, timeout=20)
    solr_conn.request('GET', solr_address)
    response = solr_conn.getresponse()
    response_json = json.loads(response.read().decode('utf-8'))
    if 'error' in response_json:
        raise ValueError('Solr error in {}'.format(response_json))
    # Only notify omnivore2 if we succeeded.
    redis.lpush('queue:index_query', solr_address) # push it for omnivore2 to catch and process
    debug('Solr responded for {}: {}'.format(scan_query, response_json))
    context['result'] = response_json['response']['docs']

    # Add the field value stats to the context.
    context['field_stats'] = {}
    if context['result']: # don't collect stats for 0 documents
        for code, field_name in settings.SOLR_FEATURE_CODES.items():
            if 'date' in field_name:
                min_date = datetime.strptime(response_json['stats']['stats_fields'][field_name]['min'],
                         '%Y-%m-%dT%H:%M:%SZ')
                max_date = datetime.strptime(response_json['stats']['stats_fields'][field_name]['max'],
                         '%Y-%m-%dT%H:%M:%SZ')
                context['field_stats'][field_name] = { 'code': code,
                        'full_name': ' '.join(field_name.split('_')[:-1]),
                        'kind': 'date',
                        'min': min_date.strftime('%m/%d/%Y'),
                        'max': max_date.strftime('%m/%d/%Y') }
            else:
                context['field_stats'][field_name] = { 'code': code,
                        'full_name': ' '.join(field_name.split('_')[:-1]),
                        'kind': 'number',
                        'min': '{:.2f}'.format(float(response_json['stats']['stats_fields'][field_name]['min'])),
                        'max': '{:.2f}'.format(float(response_json['stats']['stats_fields'][field_name]['max'])) }
    return context
