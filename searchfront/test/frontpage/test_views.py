from flask import url_for
import pytest

from searchfront.test.conftest import TEST_USER_EMAIL, TEST_USER_PASSWORD
from searchfront.blueprints.frontpage.forms import PublicScanForm
from searchfront.blueprints.site import Tag
from searchfront.blueprints.scan_schedule import ScanJob, ScanPermission
from searchfront.blueprints.scan_schedule.control import terminate_scan
from searchfront.blueprints.live_config import LiveConfigValue

def format_html_preview(text):
    text = str(text)
    container_pos = text.find('container')
    if container_pos != -1:
        return text[container_pos:container_pos+3000]
    else:
        return text[:3000]

def delete_existing_job(job_id, db):
    """
    Delete the previous test job if it exists.
    """
    existing_job = ScanJob.query.get(job_id)
    if existing_job is not None:
        db.session.delete(existing_job)
        db.session.commit()

def sample_job_id_form_data():
    job_id = ScanJob.identifier('', 'test query phrase', 'fun,games')
    # These tags should be guaranteed to be present thanks to conftest initialization.
    fun_tag_id = list(Tag.query.filter_by(name='fun'))[0].id
    games_tag_id = list(Tag.query.filter_by(name='games'))[0].id
    form_data = {
        'scan_query': 'test query phrase',
        'query_tags': [fun_tag_id, games_tag_id]
        }
    return job_id, form_data

@pytest.fixture(scope='function')
def always_issue_perms(db):
    # Temporarily configure the app to always issue guest permissions.
    free_perms_threshold = LiveConfigValue.query.get('guest_scan_permissions_threshold')
    current_free_threshold = int(free_perms_threshold.value)
    free_perms_threshold.value = 0
    db.session.add(free_perms_threshold)
    db.session.commit()

    yield True

    # Reset the threshold to the previous value.
    free_perms_threshold = LiveConfigValue.query.get('guest_scan_permissions_threshold')
    free_perms_threshold.value = current_free_threshold
    db.session.add(free_perms_threshold)
    db.session.commit()

@pytest.fixture(scope='function')
def never_issue_perms(db):
    # Temporarily configure the app to never issue guest permissions.
    free_perms_threshold = LiveConfigValue.query.get('guest_scan_permissions_threshold')
    current_free_threshold = int(free_perms_threshold.value)
    free_perms_threshold.value = 2 * int(LiveConfigValue.query.get(
        'concurrent_jobs_allowed').value)
    db.session.add(free_perms_threshold)
    db.session.commit()

    yield True

    # Reset the threshold to the previous value.
    free_perms_threshold = LiveConfigValue.query.get('guest_scan_permissions_threshold')
    free_perms_threshold.value = current_free_threshold
    db.session.add(free_perms_threshold)
    db.session.commit()

class TestFrontpageViews(object):
    def test_scanresults_registered(self, app, db, example_user):
        # Delete remaining guest permissions.
        for perm in ScanPermission.query.filter(ScanPermission.user_ip is not None,
                ScanPermission.is_used == False):
            db.session.delete(perm)
            db.session.commit()

        job_id, form_data = sample_job_id_form_data()
        delete_existing_job(job_id, db)
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
            test_job = ScanJob.query.get(job_id)
            assert test_job is not None
            terminate_scan(test_job.id)
            response_txt = response.data.decode('utf-8')
            assert 'Scan status' in response_txt
            assert not 'Sorry!' in response_txt

    def test_scanresults_guest_with_capacity(self, app, db, always_issue_perms):
        # Delete remaining guest permissions.
        for perm in ScanPermission.query.filter(ScanPermission.user_ip is not None,
                ScanPermission.is_used == False):
            db.session.delete(perm)
            db.session.commit()

        job_id, form_data = sample_job_id_form_data()
        delete_existing_job(job_id, db)
        with app.test_request_context():
            test_client = app.test_client()

            test_client.get(url_for('frontpage.home')) # should issue a permission
            test_form = PublicScanForm(data=form_data)
            response = test_client.get(url_for(
                'frontpage.scanresults',
                is_scan=True,
                **{field.name : field.object_data for field in test_form}
                ))
            test_job = ScanJob.query.get(job_id)
            assert test_job is not None
            terminate_scan(test_job.id)
            response_txt = response.data.decode('utf-8')
            assert 'Scan status' in response_txt
            assert not 'Sorry!' in response_txt

    def test_scanresults_guest_no_capacity(self, app, db, never_issue_perms):
        # Delete remaining guest permissions.
        for perm in ScanPermission.query.filter(ScanPermission.user_ip is not None,
                ScanPermission.is_used == False):
            db.session.delete(perm)
            db.session.commit()

        job_id, form_data = sample_job_id_form_data()
        delete_existing_job(job_id, db)
        with app.test_request_context():
            test_client = app.test_client()

            test_client.get(url_for('frontpage.home')) # should not issue a permission
            test_form = PublicScanForm(data=form_data)
            response = test_client.get(url_for(
                'frontpage.scanresults',
                is_scan=True,
                **{field.name : field.object_data for field in test_form}
                ))
            test_job = ScanJob.query.get(job_id)
            assert test_job is None # should NOT succeed
            response_txt = response.data.decode('utf-8')
            assert 'Sorry!' in response_txt
            assert not 'Scan status' in response_txt
