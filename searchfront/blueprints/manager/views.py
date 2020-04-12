from flask import Blueprint, redirect, url_for, request
from flask_admin import AdminIndexView
from flask_admin.form import SecureForm
from flask_admin.contrib.sqla import ModelView
from flask_security import current_user

manager = Blueprint('manager', __name__, template_folder='templates')

class ManagerView():
    """
    A generic ModelView class for any manager model.
    """
    form_base_class = SecureForm
    edit_template = 'manager/admin_edit.html'
    create_template = 'manager/admin_create.html'

    def _handle_view(self, name):
        if not self.is_accessible():
            # Redirect the user to the requested page after the successful login.
            return redirect(url_for('security.login', next=url_for(request.endpoint)))

class ManagerLoginRequired():
    def is_accessible(self):
        return (current_user.is_active and
                current_user.is_authenticated)

class ManagerAdminRequired():
    def is_accessible(self):
        return current_user.has_role('admin')

# Our superclasses have to be first to actually overwrite the defaults.
class ManagerIndexView(ManagerView, ManagerLoginRequired, AdminIndexView):
    """
    The class for the index of the manager (admin dashboard).
    """
    pass

class ManagerAdminView(ManagerView, ManagerAdminRequired, ModelView):
    """A manager view accessible only to admins."""
    pass
