from flask import Blueprint, jsonify, request
from flask_security import current_user

from searchfront.blueprints.scan_schedule import ScanJob

scan_schedule = Blueprint('scan_schedule', __name__, template_folder='templates')

@scan_schedule.route('/my_scans')
def my_scans():
    if current_user.is_authenticated:
        return jsonify([job.id for job in list(ScanJob.query.filter_by(user_id=current_user.id))])
    else:
        return jsonify([job.id for job in list(ScanJob.query.filter_by(
            user_ip=request.remote_addr))])
