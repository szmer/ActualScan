from searchfront.extensions import db

class LiveConfigValue(db.Model):
    key = db.Column(db.String(512), primary_key=True)
    value = db.Column(db.String(16384), nullable=False)
