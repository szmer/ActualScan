# Flask-SQLAlchemy.
from flask_sqlalchemy import SQLAlchemy
db = SQLAlchemy()

# Flask-Security.
from flask_security import Security, SQLAlchemyUserDatastore
from searchfront.blueprints.user import AppUser, Role
user_datastore = SQLAlchemyUserDatastore(db, AppUser, Role)
security = Security()
