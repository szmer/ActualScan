# Flask-DebugToolbar
from flask_debugtoolbar import DebugToolbarExtension
debug_toolbar = DebugToolbarExtension()

# Flask-SQLAlchemy.
from flask_sqlalchemy import SQLAlchemy
db = SQLAlchemy()

# CSRF protection.
from flask_wtf.csrf import CSRFProtect
csrf = CSRFProtect()

# Flask-Security.
from flask_security import Security, SQLAlchemyUserDatastore
from searchfront.blueprints.account import AppUser, Role
user_datastore = SQLAlchemyUserDatastore(db, AppUser, Role)
security = Security()

# Flask Admin.
from flask_admin import Admin
from searchfront.blueprints.manager import ManagerIndexView
# NOTE that the endpoints here are still named 'admin'. This prevents us from having weird errors
# with admin.static
admin = Admin(name='manager', index_view=ManagerIndexView(name='manager', url='/manager/'),
        base_template='manager/admin_base.html')

from flask_admin.contrib.sqla import ModelView
class ModelViewWithRels(ModelView):
    column_hide_backrefs = False

from searchfront.blueprints.manager import ManagerView, ManagerAdminRequired, ManagerRegisteredOnly
from searchfront.blueprints.site import Tag
class SiteModelView(ManagerView, ManagerAdminRequired, ModelViewWithRels):
    #
    # To display tags properly in the view.
    column_list = ('homepage_url', 'site_name', 'level', 'search_pointer', 'source_type', 'site_type',
            'tags')
    # We have to add the Tag.name here to make flask-admin aware of the relation?
    column_searchable_list = ('site_name', 'source_type', 'site_type', Tag.name)
    # A link could be put here with a Jinja macro, see https://flask-admin.readthedocs.io/en/v1.3.0/api/mod_model/
    # (for now this line is unnecessary, is solved with __repr__ in Tag)
    #column_formatters = dict(tags=lambda v, c, m, p: [t.name for t in m.tags])

    #
    # Customizing the form.
    form_choices = {
            'site_type': [
                ('web', 'web'), # db and display value
                ('reddit', 'reddit'),
                ],
            'source_type': [
                ('forums', 'forums'), # db and display value
                ('media', 'media'),
                ('blog', 'blog'),
                ],
            'level': [
                ('base', 'base'),
                ('community', 'community')
                ]
            }

    # See potentially these for filtering proposed tags
    # https://stackoverflow.com/questions/35139397/filtering-the-drop-downs-in-flask-admin-before-it-gets-to-the-user
    # https://stackoverflow.com/questions/40381086/how-to-customize-flask-admin-queryselectmultiplefield-choice/56462018#56462018

class SiteModelViewForRegistered(ManagerRegisteredOnly, SiteModelView):
    can_delete = False

    form_choices = {
            'site_type': [
                ('web', 'web'), # db and display value
                ('reddit', 'reddit'),
                ],
            'source_type': [
                ('forums', 'forums'), # db and display value
                ('media', 'media'),
                ('blog', 'blog'),
                ],
            'level': [
                ('community', 'community')
                ]
            }

class TagModelView(ManagerView, ManagerAdminRequired, ModelViewWithRels):
    column_list = ('name', 'description', 'level')
    form_excluded_columns = ('sites',)

    form_choices = {
            'level': [
                ('base', 'base'),
                ('community', 'community')
                ]
            }

class TagModelViewForRegistered(ManagerRegisteredOnly, TagModelView):
    can_delete = False

    form_choices = {
            'level': [
                ('community', 'community')
                ]
            }
