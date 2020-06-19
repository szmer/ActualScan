import http.client
import json
from urllib.parse import quote

from flask import Blueprint, flash, redirect, render_template, request, url_for
from flask import current_app # can be used for logging stuff if needed
from flask_security import current_user

from searchfront.blueprints.frontpage.forms import PublicScanForm
from searchfront.blueprints.scan_schedule import (request_scan, scan_progress_info,
        maybe_issue_guest_scan_permission, verify_scan_permission)

frontpage = Blueprint('frontpage', __name__, template_folder='templates')

@frontpage.route('/')
def home():
    form = PublicScanForm(formdata=request.args) # for a GET form
    if form.validate():
        return redirect(url_for('.scanresults', **request.args))
    # TODO KLUDGE currently unlimited scan permissions for registered
    if current_user.is_authenticated:
        can_scan = True
    else:
        can_scan = maybe_issue_guest_scan_permission(request.remote_addr)
    # We need the show_errors toggle for not displaying errors when there are no GET args.
    return render_template('frontpage/home.html', form=form, show_errors=bool(request.args),
            can_scan=can_scan)

def prepare_progress_info(user_id, scan_query, query_tags, is_ip=False):
    job = request_scan(user_id, scan_query, query_tags, is_ip=is_ip)
    progress_info = scan_progress_info(job.id)
    return progress_info

@frontpage.route('/results')
def scanresults():
    # Parse the form information to know what to search for.
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
    query_tags = [tag.name for tag in form.query_tags.data]

    # Is an actual scan requested?
    if 'is_scan' in request.args and request.args['is_scan']:
        scan_performed = verify_scan_permission(current_user, request.remote_addr)
        scan_finished = False
        # If the scan is being performed, check if it is finished and display progress information
        # if it's not.
        if scan_performed:
            progress_info = prepare_progress_info(
                    (current_user.id if current_user.is_authenticated else request.remote_addr),
                    scan_query, query_tags,
                    is_ip=not current_user.is_authenticated)
            if ('phase' in progress_info and progress_info['phase'] == 'finished'):
                scan_finished = True
            if not scan_finished:
                return render_template('frontpage/scanresults.html', status_data=progress_info,
                        scan_phrase=scan_query,
                        is_good=('phase' in progress_info) and (progress_info['phase']
                            not in ['rejected', 'terminated']))
        # If the scan request just failed, flash the negative infomation.
        # TODO redirect back in a UX fashion
        if not scan_performed and not scan_finished:
            flash('Sorry! We can\'t currently give you the resources to scan. Here\'s the index '
                    'results instead.', 'error')

    # The index-only search response.
    try:
        omnivore_conn = http.client.HTTPConnection('omnivore', port=4242, timeout=10)
        # TODO query tags
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
