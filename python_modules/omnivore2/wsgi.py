import http.client
import json
import socket

from flask import Flask, request

from omnivore2_conf import SOLR_HOST, SOLR_PORT, SOLR_CORE
from stationary_analysis import stationary_analysis_applied
from workers import start_workers

app = Flask('omnivore2')

@app.route('/eat/')
def eat():
    periods_to_submit = stationary_analysis_applied(request.json)
    timeouted = False
    solr_conn = http.client.HTTPConnection(SOLR_HOST, port=SOLR_PORT, timeout=15)
    try:
        solr_conn.request('GET', '/solr/{}/update'.format(SOLR_CORE),
                body=json.dumps(periods_to_submit),
                headers={'Content-type': 'application/json'})
        solr_response = solr_conn.getresponse()
    except socket.timeout:
        timeouted = True
    if timeouted or solr_response.status != 200:
        response_text = solr_response.read().decode('utf-8')
        return response_text, 500
    return 'ok'

# Start the workers for contextual analysis.
start_workers()
