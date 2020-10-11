import http.client
import json
from logging import debug, warning
import socket
import urllib

def stractor_reading(stractor_host, stractor_port, text, source_type):
    """
    Get the text of JSON response of Speechtractor for text and source_type, or HTTP status error
    code as int, or -1 on local timeout.
    """
    params = urllib.parse.urlencode({'html': text, 'sourcetype': source_type,
        'emptyurl': '1' if source_type in ['media', 'blog'] else '0'},
        quote_via=urllib.parse.quote)
    headers = {"Content-type": "application/x-www-form-urlencoded",
            "Accept": "text/json"}
    stractor_conn = http.client.HTTPConnection(stractor_host, port=stractor_port, timeout=15)
    stractor_conn.request('POST', '/api/v01/interpret', params, headers)
    try:
        stractor_response = stractor_conn.getresponse()
    except socket.timeout:
        warning('Speechtractor timed out')
        return -1
    if stractor_response.status != 200:
        warning(stractor_response.read())
        return stractor_response.status
    return stractor_response.read().decode('utf-8')

def solr_update(solr_host, solr_port, solr_core, req_body, req_id=None, req_class=False):
    """
    Send req_body as payload to Solr, optionally updating the req_id ScrapeRequest (or other given
    req_class) on failure. Return a boolean indicating success or failure.
    """
    # Recreate the connection each time to avoid getting stuck in bad states.
    solr_conn = http.client.HTTPConnection(solr_host, port=solr_port, timeout=15)
    solr_conn.request('GET', '/solr/{}/update'.format(solr_core), body=req_body,
        headers={'Content-type': 'application/json'})
    debug('Sent to Solr: {}'.format(req_body))
    timeouted = False
    try:
        solr_response = solr_conn.getresponse()
    except socket.timeout:
        timeouted = True
    if timeouted or solr_response.status != 200:
        if req_id is not None:
            db_request = req_class.objects.get(id=req_id)
            db_request.status = 'failed'
            db_request.failure_comment = 'Could not reach Solr, status code {}'.format(
                    solr_response.status)
        return False
    else:
        debug('Solr response: {}'.format(solr_response.status))
    return True

def solr_check_urls(solr_host, solr_port, solr_core, date_post_check, date_retr_check, urls):
    urls_to_skip = set()
    debug('Asking Solr about urls: {} or more'.format(urls[:20]))
    query_str = ('/solr/{}/select?fl=url&q=date_post:{}%20date_retr:{}&q=url:({})'.format(
        solr_core,
        urllib.parse.quote(date_post_check), urllib.parse.quote(date_retr_check),
        urllib.parse.quote(' '.join(urls), safe='')))
    conn = http.client.HTTPConnection(solr_host, port=solr_port)
    conn.request('GET', query_str, headers={'Content-type': 'application/json'})
    response = conn.getresponse()
    response_text = response.read().decode('utf-8')
    response_json = json.loads(response_text)
    for doc in response_json['response']['docs']:
        urls_to_skip.add(doc['url'])
    return urls_to_skip 
