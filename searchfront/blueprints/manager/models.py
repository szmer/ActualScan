from searchfront.blueprints.account import AppUser
from searchfront.lib import now_time
from searchfront.extensions import db

class ChangeRequest(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    creator_id = db.Column(db.Integer, db.ForeignKey('app_user.id'))
    creator = db.relationship(AppUser)
    subject = db.Column(db.String(1024), nullable=False)
    content = db.Column(db.String(5000), nullable=False)
    response = db.Column(db.String(5000))
    created = db.Column(db.DateTime(timezone=True), nullable=False, default=now_time)
    resolved = db.Column(db.Boolean(), nullable=False, default=False)
