# Flask-SQLAlchemy.
from flask_sqlalchemy import SQLAlchemy
db = SQLAlchemy()

# CSRF protection.
from flask_wtf.csrf import CSRFProtect
csrf = CSRFProtect()

# Flask-Security.
from flask_security import Security, SQLAlchemyUserDatastore
from searchfront.blueprints.user import AppUser, Role
user_datastore = SQLAlchemyUserDatastore(db, AppUser, Role)
security = Security()

# Flask Admin.
from flask_admin import Admin
from searchfront.blueprints.manager import ManagerIndexView
# NOTE that the endpoints here are still named 'admin'. This prevents us from having weird errors
# with admin.static
admin = Admin(name='Manager', index_view=ManagerIndexView(name='Manager', url='/manager/'))

from flask_admin.contrib.sqla import ModelView
class ModelViewWithRels(ModelView):
    column_hide_backrefs = False

from searchfront.blueprints.site import Tag
class SiteModelView(ModelViewWithRels):
    #
    # To display tags properly in the view.
    column_list = ('homepage_url', 'site_name', 'search_pointer', 'source_type', 'site_type',
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
                ]}

    # See potentially these for filtering proposed tags
    # https://stackoverflow.com/questions/35139397/filtering-the-drop-downs-in-flask-admin-before-it-gets-to-the-user
    # https://stackoverflow.com/questions/40381086/how-to-customize-flask-admin-queryselectmultiplefield-choice/56462018#56462018

class TagModelView(ModelViewWithRels):
    form_excluded_columns = ('sites',)
