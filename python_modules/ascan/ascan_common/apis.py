from base64 import b64encode
import http.client
import json
from logging import debug, warning
import socket
import urllib
import ssl

ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
ssl_context.load_verify_locations('/home/certs/ascan_internal.pem')

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

def eat_omnivore2(omniv2_host, omniv2_port, json_doc, req_id=None, req_class=False):
    """
    Send the json_doc to omnivore2, optionally updating the req_id ScrapeRequest (or other given
    req_class) on failure. Return a boolean indicating success or failure.
    """
    omniv2_conn = http.client.HTTPConnection(omniv2_host, port=omniv2_port, timeout=15)
    timeouted = False
    try:
        headers = {'Content-type': 'application/json'}
        omniv2_conn.request('GET', '/eat/', body=json.dumps(json_doc), headers=headers)
        omniv2_response = omniv2_conn.getresponse()
    except socket.timeout:
        timeouted = True
    if timeouted or omniv2_response.status != 200:
        if req_id is not None:
            db_request = req_class.objects.get(id=req_id)
            db_request.status = 'failed'
            db_request.failure_comment = 'Could not reach omnivore2, status code {}'.format(
                    omniv2_response.status)
        return False
    return True

def solr_check_urls(solr_host, solr_port, solr_core, solr_pass, date_post_check, date_retr_check,
        urls):
    urls_to_skip = set()
    debug('Asking Solr about urls: {} or more'.format(urls[:20]))
    query_str = ('/solr/{}/select?fl=url&q=date_post:{}%20date_retr:{}&q=url:({})'.format(
        solr_core,
        urllib.parse.quote(date_post_check), urllib.parse.quote(date_retr_check),
        urllib.parse.quote(' '.join(urls), safe='')))
    headers = { 'Content-type': 'application/json',
            'Authorization':
            'Basic {}'.format(b64encode(bytes('reader:'+solr_pass, 'utf-8')).decode('ascii')) }
    conn = http.client.HTTPSConnection(solr_host, port=solr_port,
            context=ssl_context)
    conn.request('GET', query_str, headers=headers)
    response = conn.getresponse()
    response_text = response.read().decode('utf-8')
    response_json = json.loads(response_text)
    for doc in response_json['response']['docs']:
        urls_to_skip.add(doc['url'])
    return urls_to_skip 
