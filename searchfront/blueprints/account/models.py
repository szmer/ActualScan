from searchfront.extensions import db

from flask_security import UserMixin, RoleMixin

roles_app_users = db.Table('roles_app_users',
        db.Column('app_user_id', db.Integer(), db.ForeignKey('app_user.id')),
        db.Column('role_id', db.Integer(), db.ForeignKey('role.id')))

class Role(db.Model, RoleMixin):
    id = db.Column(db.Integer(), primary_key=True)
    name = db.Column(db.String(80), unique=True)
    description = db.Column(db.String(255))

# User is a reserved name in Postgres.
class AppUser(db.Model, UserMixin):
    id = db.Column(db.Integer, primary_key=True)
    email = db.Column(db.String(255), unique=True)
    password = db.Column(db.String(255))
    active = db.Column(db.Boolean())
    confirmed_at = db.Column(db.DateTime())
    roles = db.relationship('Role', secondary=roles_app_users,
                            backref=db.backref('app_users', lazy='dynamic'))
