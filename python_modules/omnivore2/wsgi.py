from base64 import b64encode
import http.client
import json
import socket
import ssl

from flask import Flask, request

from omnivore2_conf import SOLR_HOST, SOLR_PORT, SOLR_CORE, SOLR_UPDATER_PASS
from stationary_analysis import stationary_analysis_applied
from workers import start_workers

app = Flask('omnivore2')

ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
ssl_context.load_verify_locations('/home/certs/ascan_internal.pem')

@app.route('/eat/', methods=['POST'])
def eat():
    # NOTE the incoming json has to be a list of documents
    periods_to_submit = stationary_analysis_applied(request.json)
    timeouted = False
    headers = { 'Content-type': 'application/json',
            'Authorization':
            'Basic {}'.format(b64encode(bytes('updater:'+SOLR_UPDATER_PASS, 'utf-8')).decode('ascii'))
            }
    solr_conn = http.client.HTTPSConnection(SOLR_HOST,
            port=SOLR_PORT, timeout=15, context=ssl_context)
    try:
        solr_conn.request('GET', '/solr/{}/update?commit=true'.format(SOLR_CORE),
                body=json.dumps(periods_to_submit),
                headers=headers)
        solr_response = solr_conn.getresponse()
    except socket.timeout:
        timeouted = True
    if timeouted or solr_response.status != 200:
        response_text = solr_response.read().decode('utf-8')
        return response_text, 500
    return 'ok'

# Start the workers for contextual analysis.
start_workers()
