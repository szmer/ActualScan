import json

from flask import url_for

from searchfront.blueprints.frontpage.forms import PublicScanForm
from searchfront.test.conftest import TEST_USER_EMAIL, TEST_USER_PASSWORD
from searchfront.test.frontpage.test_views import sample_form_data, delete_existing_jobs

class TestScanScheduleEvents(object):
    def test_socket_guest(self, app, socketio, db, always_issue_perms):
        form_data = sample_form_data()
        delete_existing_jobs(db)
        with app.test_request_context():
            test_client = app.test_client()

            test_client.get(url_for('frontpage.home')) # should issue a permission
            test_form = PublicScanForm(data=form_data)
            test_client.get(url_for(
                'frontpage.scanresults',
                is_scan=True,
                **{field.name : field.object_data for field in test_form}
                ))

            # Try to get the scan ID by the API.
            response = test_client.get(url_for('scan_schedule.my_scans'))
            jobs = json.loads(response.data)
            assert len(jobs) == 1
            job_id = jobs[0]

            # Test the WebSocket reporting.
            socketio_test_client = socketio.test_client(app, flask_test_client=test_client)
            assert socketio_test_client.is_connected()
            socketio_test_client.emit('scan_status', {'job_id': job_id})
            response = socketio_test_client.get_received()
            assert len(response) == 1
            response = response[0]
            assert response['name'] == 'scan_status'
            assert 'args' in response
            assert 'phase' in response['args'][0]

    def test_socket_registered(self, app, socketio, db):
        form_data = sample_form_data()
        delete_existing_jobs(db)
        with app.test_request_context():
            test_client = app.test_client()

            # Login.
            response = test_client.post(url_for('security.login'), data={'email': TEST_USER_EMAIL,
                'password': TEST_USER_PASSWORD})
            assert 'Redirecting...' in response.data.decode('utf-8') # means success

            test_client.get(url_for('frontpage.home')) # should issue a permission
            test_form = PublicScanForm(data=form_data)
            test_client.get(url_for(
                'frontpage.scanresults',
                is_scan=True,
                **{field.name : field.object_data for field in test_form}
                ))

            # Try to get the scan ID by the API.
            response = test_client.get(url_for('scan_schedule.my_scans'))
            jobs = json.loads(response.data)
            assert len(jobs) == 1
            job_id = jobs[0]

            socketio_test_client = socketio.test_client(app, flask_test_client=test_client)
            assert socketio_test_client.is_connected()
            socketio_test_client.emit('scan_status', {'job_id': job_id})
            response = socketio_test_client.get_received()
            assert len(response) == 1
            response = response[0]
            assert response['name'] == 'scan_status'
            assert 'args' in response
            assert 'phase' in response['args'][0]
