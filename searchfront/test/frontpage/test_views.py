from flask import url_for

from searchfront.test.conftest import TEST_USER_EMAIL, TEST_USER_PASSWORD
from searchfront.blueprints.frontpage.forms import PublicScanForm
from searchfront.blueprints.site import Tag
from searchfront.blueprints.scan_schedule import ScanJob, ScanPermission
from searchfront.blueprints.scan_schedule.control import terminate_scan

def format_html_preview(text):
    text = str(text)
    container_pos = text.find('container')
    if container_pos != -1:
        return text[container_pos:container_pos+3000]
    else:
        return text[:3000]

def existing_jobs(db):
    return list(ScanJob.query.filter_by(query_phrase='test query phrase',
        query_tags='fun,games'))

def delete_existing_jobs(db):
    """
    Delete the previous test job if it exists.
    """
    jobs = existing_jobs(db)
    if jobs:
        for job in jobs:
            db.session.delete(job)
            db.session.commit()

def sample_form_data():
    # These tags should be guaranteed to be present thanks to conftest initialization.
    fun_tag_id = list(Tag.query.filter_by(name='fun'))[0].id
    games_tag_id = list(Tag.query.filter_by(name='games'))[0].id
    form_data = {
        'scan_query': 'test query phrase',
        'query_tags': [fun_tag_id, games_tag_id]
        }
    return form_data

class TestFrontpageViews(object):
    def test_scanresults_registered(self, app, db, example_user):
        # Delete remaining guest permissions.
        for perm in ScanPermission.query.filter(ScanPermission.user_ip is not None,
                ScanPermission.is_used == False):
            db.session.delete(perm)
            db.session.commit()

        form_data = sample_form_data()
        delete_existing_jobs(db)
        with app.test_request_context():
            test_client = app.test_client()
            # Login.
            response = test_client.post(url_for('security.login'), data={'email': TEST_USER_EMAIL,
                'password': TEST_USER_PASSWORD})
            assert 'Redirecting...' in response.data.decode('utf-8') # means success

            test_form = PublicScanForm(data=form_data)
            response = test_client.get(url_for(
                'frontpage.scanresults',
                is_scan=True,
                # object_data is "data passed from an object or from kwargs to the field, stored
                # unmodified"
                **{field.name : field.object_data for field in test_form}
                ))
            test_jobs = existing_jobs(db)
            assert test_jobs
            terminate_scan(test_jobs[0].id)
            response_txt = response.data.decode('utf-8')
            assert 'Scan status' in response_txt
            assert not 'Sorry!' in response_txt

    def test_scanresults_guest_with_capacity(self, app, db, always_issue_perms):
        # Delete remaining guest permissions.
        for perm in ScanPermission.query.filter(ScanPermission.user_ip is not None,
                ScanPermission.is_used == False):
            db.session.delete(perm)
            db.session.commit()

        form_data = sample_form_data()
        delete_existing_jobs(db)
        with app.test_request_context():
            test_client = app.test_client()

            test_client.get(url_for('frontpage.home')) # should issue a permission
            test_form = PublicScanForm(data=form_data)
            response = test_client.get(url_for(
                'frontpage.scanresults',
                is_scan=True,
                **{field.name : field.object_data for field in test_form}
                ))
            test_jobs = existing_jobs(db)
            assert test_jobs
            terminate_scan(test_jobs[0].id)
            response_txt = response.data.decode('utf-8')
            assert 'Scan status' in response_txt
            assert not 'Sorry!' in response_txt

    def test_scanresults_guest_no_capacity(self, app, db, never_issue_perms):
        # Delete remaining guest permissions.
        for perm in ScanPermission.query.filter(ScanPermission.user_ip is not None,
                ScanPermission.is_used == False):
            db.session.delete(perm)
            db.session.commit()

        form_data = sample_form_data()
        delete_existing_jobs(db)
        with app.test_request_context():
            test_client = app.test_client()

            test_client.get(url_for('frontpage.home')) # should not issue a permission
            test_form = PublicScanForm(data=form_data)
            response = test_client.get(url_for(
                'frontpage.scanresults',
                is_scan=True,
                **{field.name : field.object_data for field in test_form}
                ))
            test_jobs = existing_jobs(db)
            assert not test_jobs
            response_txt = response.data.decode('utf-8')
            assert 'Sorry!' in response_txt
            assert not 'Scan status' in response_txt
