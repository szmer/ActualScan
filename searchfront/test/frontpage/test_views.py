from flask import url_for

from searchfront.blueprints.frontpage.forms import PublicScanForm
from searchfront.blueprints.site import Tag
from searchfront.blueprints.scan_schedule import ScanJob
from searchfront.blueprints.scan_schedule.control import terminate_scan

class TestFrontpageViews(object):
    def test_scanresults(self, app, db):
        # Delete the previous job if it exists.
        job_id = ScanJob.identifier('', 'test query phrase', 'fun,games')
        existing_job = ScanJob.query.get(job_id)
        if existing_job is not None:
            db.session.delete(existing_job)
            db.session.commit()
        # These tags should be guaranteed to be present thanks to conftest initialization.
        fun_tag_id = list(Tag.query.filter_by(name='fun'))[0].id
        games_tag_id = list(Tag.query.filter_by(name='games'))[0].id
        with app.test_request_context():
            form_data = {
                'scan_query': 'test query phrase',
                'query_tags': [fun_tag_id, games_tag_id]
                }
            test_form = PublicScanForm(data=form_data)
            test_client = app.test_client()
            test_client.get(url_for(
                'frontpage.scanresults',
                # object_data is "data passed from an object or from kwargs to the field, stored
                # unmodified"
                **{field.name : field.object_data for field in test_form}
                ))
            # We establish empty ('') user for guests.
            test_job = ScanJob.query.get(job_id)
            assert test_job is not None
            terminate_scan(test_job.id)
