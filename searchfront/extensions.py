# Flask-SQLAlchemy.
from flask_sqlalchemy import SQLAlchemy
db = SQLAlchemy()

# Flask-Security.
from flask_security import Security, SQLAlchemyUserDatastore
from searchfront.blueprints.user import AppUser, Role
user_datastore = SQLAlchemyUserDatastore(db, AppUser, Role)
security = Security()

# Flask Admin.
from flask_admin import Admin
from searchfront.blueprints.manager import ManagerView
# NOTE that the endpoints here are still under 'admin'. This prevents us from having weird errors
# with admin.static
admin = Admin(name='Manager', index_view=ManagerView(name='Manager', url='/manager/'))
