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
        data_resp = { 'phase': 'Bad request failure.' }
        return render_template('frontpage/scanresults.html', status_data=data_resp,
                is_good=False), 400
    ##-current_app.logger.info('query tags in form: {}'.format(form.query_tags.data))
    ##-current_app.logger.info('tag type in form: {}'.format(type(form.query_tags.data[0])))
    ##=tag_names = [tag.name for tag in list(Tag.query.filter(Tag.name.in_(form.query_tags.data)))]
    # NOTE We establish empty ('') user for guests.
    # NOTE 2 the tags in form should be auto-converted to Tag objects.
    job = request_scan('', form.scan_query.data,
            ','.join([tag.name for tag in form.query_tags.data]))
    do_scan_management() # KLUDGE KLUDGE
    progress_info = scan_progress_info(job)
    return render_template('frontpage/scanresults.html', status_data=progress_info,
            is_good=('phase' in progress_info)
            and (progress_info['phase'] not in ['rejected', 'terminated']))
