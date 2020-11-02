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

def rules_results(query_phrase, rules, query_site_names='', highlight=False):
    context = dict()
    context['rules_string'] = rules
    context['rules'] = {}
    solr_filter_terms = []
    solr_boost_terms = []
    # Collect the rules to translate them to Solr query parameters.
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
            try:
                if float(boost_term) != 0:
                    solr_boost_terms.append('sum(0.001,mul({}, {}))'.format(field_name, boost_term))
            except ValueError:
                pass
            context['rules'][field_name] = { 'min': min_term, 'max': max_term,
                    'weight': boost_term }

    # Make an AND query by default. ( TODO set it in Solr?)
    if not ' OR ' in query_phrase and not ' AND ' in query_phrase:
        query_phrase = query_phrase.replace(' ', ' AND ')

    solr_address = '/solr/{}/select'.format(settings.SOLR_CORE)
    solr_address += '?defType=edismax&q={}&qf={}'.format(parse.quote(query_phrase, safe=''),
            parse.quote(' '.join(settings.SOLR_TEXT_FIELDS), safe=''))
    if query_site_names:
        solr_address += '&' + parse.quote(' '.join(query_site_names), safe='')
    for filter_term in solr_filter_terms:
        solr_address += '&fq=' + parse.quote(filter_term, safe='')
    for boost_term in solr_boost_terms:
        solr_address += '&boost=' + parse.quote(boost_term, safe='')
    solr_address += '&stats=true'
    for code, field_name in settings.SOLR_FEATURE_CODES.items():
        solr_address += '&stats.field='+field_name
    if highlight:
        solr_address += '&hl=true&hl.method=unified&hl.fl={}&hl.tag.pre=**&hl.tag.post=**'.format(
                ','.join(settings.SOLR_TEXT_FIELDS))
    info('Constructed Solr query {}'.format(solr_address))
    solr_conn = http.client.HTTPConnection(settings.SOLR_HOST, port=settings.SOLR_PORT, timeout=20)
    solr_conn.request('GET', solr_address)
    response = solr_conn.getresponse()
    response_json = json.loads(response.read().decode('utf-8'))
    if 'error' in response_json:
        raise ValueError('Solr error in {}'.format(response_json))
    # Only notify omnivore2 if we succeeded.
    redis.lpush('queue:index_query', solr_address) # push it for omnivore2 to catch and process
    debug('Solr responded for {}: {}'.format(query_phrase, response_json))
    context['result'] = response_json['response']['docs']

    # Intergate the highlighting info from Solr into the results.
    if highlight:
        for doc_n, doc in enumerate(context['result']):
            hl_entry = response_json['highlighting'][doc['doc_location']]
            for field_name in settings.SOLR_TEXT_FIELDS:
                if field_name in doc and doc[field_name]:
                    for hl_snippet in hl_entry[field_name]:
                        context['result'][doc_n][field_name] = (
                                # find the version without highlight markings and replace it with the
                                # highlighted version
                                context['result'][doc_n][field_name].replace(
                                    hl_snippet.replace('**', ''), hl_snippet))
                    break
                    
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
                        'full_name': settings.HUMAN_FEATURE_NAMES[code],
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
