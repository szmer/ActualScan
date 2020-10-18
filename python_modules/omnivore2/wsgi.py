import http.client
import json

from flask import Flask, request

from omnivore2_conf import SOLR_HOST, SOLR_PORT, SOLR_CORE
from stationary_analysis import stationary_analysis_applied

app = Flask('omnivore2')

@app.route('/eat/')
def eat():
    periods_to_submit = stationary_analysis_applied(request.json)
    solr_conn = http.client.HTTPConnection(SOLR_HOST, port=SOLR_PORT, timeout=15)
    solr_conn.request('GET', '/solr/{}/update'.format(SOLR_CORE),
            body=json.dumps(periods_to_submit),
            headers={'Content-type': 'application/json'})
    return 'ok'
