from flask import redirect, url_for
from flask_admin import AdminIndexView
from flask_security import current_user

class ManagerView(AdminIndexView):
    # We have to replace this with the global static place to not prevent tryig to access a
    # separate static folder for admin.
    static_folder = 'static'

    def is_accessible(self):
        return (current_user.is_active and
                current_user.is_authenticated)

    def _handle_view(self, name):
        if not self.is_accessible():
            # Redirect the user to the main Manager page after the successful login.
            return redirect(url_for('security.login', next=url_for('admin.index')))
