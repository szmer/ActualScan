from flask import request
from flask_socketio import emit
from flask_security import current_user

from searchfront.extensions import socketio
from searchfront.blueprints.scan_schedule import verify_scan_permission, scan_progress_info

@socketio.on('scan_status')
def handle_scan_progress(json):
    job_id = json['job_id']
    # Verify that the client has authorization to view this job.
    # TODO are we loading the job twice from the db? maybe optimize
    has_perm = verify_scan_permission(current_user, request.remote_addr, specific_id=job_id)
    if has_perm:
        progress_info = scan_progress_info(job_id)
        emit('scan_status', progress_info)
    else:
        emit('access_denied', 'denied')
