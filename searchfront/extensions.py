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
from searchfront.blueprints.manager import ManagerIndexView
# NOTE that the endpoints here are still under 'admin'. This prevents us from having weird errors
# with admin.static
admin = Admin(name='Manager', index_view=ManagerIndexView(name='Manager', url='/manager/'))

from flask_admin.contrib.sqla import ModelView
class ModelViewWithRels(ModelView):
    column_hide_backrefs = False

from searchfront.blueprints.site import Tag
class SiteModelView(ModelViewWithRels):
    column_list = ('homepage_url', 'site_name', 'search_pointer', 'source_type', 'site_type',
            'tags')
    # We have to add the Tag.name here to make flask-admin aware of the relation?
    column_searchable_list = ('site_name', 'source_type', 'site_type', Tag.name)
    # A link could be put here with a Jinja macro, see https://flask-admin.readthedocs.io/en/v1.3.0/api/mod_model/
    column_formatters = dict(tags=lambda v, c, m, p: [t.name for t in m.tags])
