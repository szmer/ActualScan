import http.client
import json
from urllib.parse import quote

from flask import Blueprint, redirect, render_template, request, url_for
from flask import current_app # can be used for logging stuff if needed

from searchfront.blueprints.frontpage.forms import PublicScanForm
from searchfront.blueprints.scan_schedule import do_scan_management, request_scan, scan_progress_info

frontpage = Blueprint('frontpage', __name__, template_folder='templates')

@frontpage.route('/')
def home():
    form = PublicScanForm(formdata=request.args) # for a GET form
    if form.validate():
        return redirect(url_for('.scanresults', **request.args))
    return render_template('frontpage/home.html', form=form, show_errors=bool(request.args))

@frontpage.route('/scanresults')
def scanresults():
    form = PublicScanForm(formdata=request.args) # instatiate from GET arguments
    if not form.validate():
        for field in form:
            if field.errors:
                current_app.logger.info('An error in scan argument {}, {}: {}'.format(
                    field.name, field.data, field.errors))
        # Make a short dict compatible with what scan_progress_info would return.
        error_data_resp = { 'phase': 'Bad request failure.' }
        return render_template('frontpage/scanresults.html', status_data=error_data_resp,
                is_good=False), 400
    scan_query = form.scan_query.data
    # TODO TODO KLUDGE DANGER Make *making* scan authorization-limited!!!
    if not 'skip_scan' in request.args or not request.args['skip_scan']:
        # NOTE We establish empty ('') user for guests.
        # NOTE 2 the tags in form should be aut-converted to Tag objects.
        job = request_scan('', scan_query,
                ','.join([tag.name for tag in form.query_tags.data]))
        do_scan_management() # KLUDGE KLUDGE (no rate limiting here!)
        progress_info = scan_progress_info(job)
    if (('skip_scan' in request.args and request.args['skip_scan'])
        or ('phase' in progress_info and progress_info['phase'] == 'finished')):
        try:
            omnivore_conn = http.client.HTTPConnection('omnivore', port=4242, timeout=10)
            omnivore_conn.request('GET', '/result?q={}'.format(quote(scan_query)),
                    headers={'Content-type': 'application/json'})
            omnivore_response = omnivore_conn.getresponse()
            omnivore_results = json.loads(omnivore_response.read())
            current_app.logger.debug('Omnivore responded for {}: {}'.format(scan_query,
                omnivore_results))
            return render_template('frontpage/scanresults.html',
                    result=omnivore_results,
                    scan_phrase=scan_query, is_good=True)
        except Exception as e:
            current_app.logger.info('Error processing scan/search request: {}'.format(e))
            error_data_resp = { 'phase': 'Internal error when processing the request.' }
            return render_template('frontpage/scanresults.html', status_data=error_data_resp,
                    is_good=False), 500
    else:
        return render_template('frontpage/scanresults.html', status_data=progress_info,
                scan_phrase=scan_query,
                is_good=('phase' in progress_info) and (progress_info['phase']
                    not in ['rejected', 'terminated']))
