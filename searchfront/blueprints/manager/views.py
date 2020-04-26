from flask import Blueprint, flash, redirect, render_template, request, url_for
from flask_admin import AdminIndexView
from flask_admin.contrib.sqla import ModelView
from flask_security import current_user
from wtforms.validators import ValidationError

from searchfront.extensions import db
from searchfront.blueprints.site import Site, Tag
from searchfront.blueprints.manager.models import ChangeRequest
from searchfront.blueprints.manager.forms import ChangeRequestForm

manager = Blueprint('manager', __name__, template_folder='templates')

@manager.route('/change_request')
def add_change_request():
    # Get the form data.
    if request.method == 'GET':
        form = ChangeRequestForm(formdata=request.args)
    else:
        form = ChangeRequestForm(formdata=request.form)

    # Extract the object in question.
    tag, site = None, None
    try:
        subject_type, id = form.extract_subject_id(form.subject.data)
        if subject_type == 'site':
            site = Site.query.get(id)
        if subject_type == 'tag':
            tag = Tag.query.get(id)
    except ValidationError:
        pass

    # Put the request into the database if the form validates.
    if form.validate():
        change_req = ChangeRequest(subject=repr(site or tag), content=form.content.data,
                creator=current_user)
        db.session.add(change_req)
        db.session.commit()
        flash('Thank you for your report!')
        if tag:
            return redirect(url_for('manager_tag.index_view'))
        else:
            return redirect(url_for('manager_site.index_view'))

    # Show the form with info on the object about which there is the change request.
    return render_template('manager/add_change_request.html', form=form, tag=tag, site=site,
            show_errors=bool(form.content.data))

class ManagerView():
    """
    A generic ModelView class for any manager model.
    """
    # NOTE FlaskWTF used by Flask-Admin already include CSRF protection, so flask_admin's SecureForm
    # only breaks this by duplicating csrf_token fields
    #form_base_class = SecureForm

    list_template = 'manager/admin_list.html'
    edit_template = 'manager/admin_edit.html'
    create_template = 'manager/admin_create.html'

    def _handle_view(self, name):
        if not self.is_accessible():
            # Redirect the user to the requested page after the successful login.
            return redirect(url_for('security.login', next=url_for(request.endpoint)))

class ManagerLoginRequired():
    """A helper class to create views accessible only to users that are logged in"""
    def is_accessible(self):
        return (current_user.is_active and
                current_user.is_authenticated)

class ManagerAdminRequired():
    """A helper class to create views accessible only to admins."""
    def is_accessible(self):
        return current_user.has_role('admin')

class ManagerRegisteredOnly():
    """
    A helper class to create views accessible only to users with the registered role (with
    permissions appropriate for them).
    """
    def is_accessible(self):
        return current_user.has_role('registered') or current_user.has_role('admin')

# Our superclasses have to be first to actually overwrite the defaults.
class ManagerIndexView(ManagerView, ManagerLoginRequired, AdminIndexView):
    """
    The class for the index of the manager (admin dashboard).
    """
    pass

class ManagerAdminView(ManagerView, ManagerAdminRequired, ModelView):
    """A manager view accessible only to admins."""
    can_create = True
    can_delete = True
    can_edit = True

    column_display_pk = True

    can_view_details = True
