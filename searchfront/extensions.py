# Flask-SocketIO
from flask_socketio import SocketIO
socketio = SocketIO()

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
admin = Admin(name='home', index_view=ManagerIndexView(name='manager', url='/manager/'),
        base_template='manager/admin_base.html')

from flask_admin.contrib.sqla import ModelView
class ModelViewWithRels(ModelView):
    column_hide_backrefs = False

from flask_admin.contrib.sqla import ModelView
from searchfront.blueprints.manager import (ManagerView, ManagerAdminRequired,
        ManagerRegisteredOnly)
from searchfront.blueprints.site import (Tag, tag_level_formatter, site_level_formatter,
        tag_level_formatter_admin, site_level_formatter_admin,)
class SiteModelView(ManagerView, ManagerAdminRequired, ModelViewWithRels):
    #
    # To display tags properly in the view.
    column_list = ('homepage_url', 'site_name', 'creator', 'level', 'search_pointer', 'source_type',
            'site_type', 'tags')
    # We have to add the Tag.name here to make flask-admin aware of the relation?
    column_searchable_list = ('site_name', 'source_type', 'site_type', Tag.name)
    # A link could be put here with a Jinja macro, see https://flask-admin.readthedocs.io/en/v1.3.0/api/mod_model/
    # (for now this line is unnecessary, is solved with __repr__ in Tag)
    #column_formatters = dict(tags=lambda v, c, m, p: [t.name for t in m.tags])

    column_formatters = dict(level=site_level_formatter_admin)

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
                (100000, 'base'),
                (10, 'community')
                ]
            }
    form_args = {
            'homepage_url': {
                'description': 'Copy the address of the site\'s main page here.'
                },
            'search_pointer': {
                'description': 'Paste this text into the search option on the site:\n'
                '|||fat||| |||cat|||\n - then copy the URL here.'
                }
            }

    # See potentially these for filtering proposed tags
    # https://stackoverflow.com/questions/35139397/filtering-the-drop-downs-in-flask-admin-before-it-gets-to-the-user
    # https://stackoverflow.com/questions/40381086/how-to-customize-flask-admin-queryselectmultiplefield-choice/56462018#56462018

class SiteModelViewForRegistered(ManagerRegisteredOnly, SiteModelView):
    can_delete = False
    can_edit = False

    list_template = 'manager/admin_list_site_msgs.html'

    #column_exclude_list = ['search_pointer', ]

    column_formatters = dict(level=site_level_formatter)

    form_excluded_columns = ['level', 'creator']

class TagModelView(ManagerView, ManagerAdminRequired, ModelViewWithRels):
    column_list = ('name', 'description', 'level', 'creator')
    form_excluded_columns = ('sites',)

    column_formatters = dict(level=tag_level_formatter_admin)

    form_choices = {
            'level': [
                (100000, 'base'),
                (10, 'community')
                ]
            }
    form_args = {
            'description': {
                'description': 'You can tell others how the tag should be used.'
                }
            }

class TagModelViewForRegistered(ManagerRegisteredOnly, TagModelView):
    can_delete = False
    can_edit = False

    list_template = 'manager/admin_list_tag_msgs.html'

    column_formatters = dict(level=tag_level_formatter)

    form_excluded_columns = ['level', 'sites', 'creator']

class ChangeRequestModelView(ManagerView, ManagerAdminRequired, ModelView):
    pass
    #columns = ('creator_id', 'subject', 'content', 'created', 'resolved')
