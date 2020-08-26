from datetime import datetime, timezone
import http.client
import json
from logging import debug, warning
import urllib
from urllib.parse import urlparse

def date_fmt(time_obj):
    return time_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def timestamp_now():
    return date_fmt(datetime.now(tz=timezone.utc))

def full_url(part, site_url):
    """
    If the url lacks the domain or scheme part, fill it in using the site url.
    """
    part_parsed = urlparse(part)
    if part_parsed.netloc and part_parsed.scheme:
        return part
    site_url_parsed = urlparse(site_url)
    part_parsed = part_parsed._replace(netloc=site_url_parsed.netloc)
    part_parsed = part_parsed._replace(scheme=site_url_parsed.scheme)
    return part_parsed.geturl()

#
# Page filing.
#
def write_to_file(path, content):
    with open(path, 'w+') as out_file:
        print(content, file=out_file)

#
# API communications. 
#
def stractor_reading(text, source_type):
    """
    Get the text of JSON response of Speechtractor for text and source_type, or HTTP status error
    code as int.
    """
    params = urllib.parse.urlencode({'html': text, 'sourcetype': source_type,
        'emptyurl': '1' if source_type in ['media', 'blog'] else '0'},
        quote_via=urllib.parse.quote)
    headers = {"Content-type": "application/x-www-form-urlencoded",
            "Accept": "text/json"}
    stractor_conn = http.client.HTTPConnection('speechtractor', port=3756, timeout=5)
    stractor_conn.request('POST', '/api/v01/interpret', params, headers)
    stractor_response = stractor_conn.getresponse()
    if stractor_response.status != 200:
        warning(stractor_response.read())
        return stractor_response.status
    return stractor_response.read().decode('utf-8')

def solr_update(req_body, req_id=None, req_class=False):
    """
    Send req_body as payload to Solr, optionally updating the req_id ScrapeRequest (or other given
    req_class) on failure. Return a boolean indicating success or failure.
    """
    # Recreate the connection each time to avoid getting stuck in bad states.
    solr_conn = http.client.HTTPConnection('solr', port=8983, timeout=10)
    solr_conn.request('GET', '/solr/lookupy/update', body=req_body,
        headers={'Content-type': 'application/json'})
    debug('Sent to Solr: {}'.format(req_body))
    solr_response = solr_conn.getresponse()
    debug('Solr response: {}'.format(solr_response.status))
    if solr_response.status != 200:
        if req_id is not None:
            db_request = req_class.objects.get(id=req_id)
            db_request.status = 'failed'
            db_request.failure_comment = 'Could not reach Solr, status code {}'.format(
                    solr_response.status)
        return False
    return True

def solr_check_urls(date_post_check, date_retr_check, urls):
    urls_to_skip = set()
    debug('Asking Solr about urls: {} or more'.format(urls[:20]))
    query_str = ('/solr/lookupy/select?fl=url&q=date_post:{}%20date_retr:{}&q=url:({})'.format(
        urllib.parse.quote(date_post_check), urllib.parse.quote(date_retr_check),
        urllib.parse.quote(' '.join(urls), safe='')))
    conn = http.client.HTTPConnection('solr', port=8983)
    conn.request('GET', query_str, headers={'Content-type': 'application/json'})
    response = conn.getresponse()
    response_text = response.read().decode('utf-8')
    response_json = json.loads(response_text)
    for doc in response_json['response']['docs']:
        urls_to_skip.add(doc['url'])
    return urls_to_skip 

def update_request_status(request, new_status, failure_comment=None):
    request.change_status(new_status)
    if failure_comment is not None:
        request.failure_comment = failure_comment
    request.save()
