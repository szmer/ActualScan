from flask import Blueprint, render_template
from flask_security import login_required, current_user

account = Blueprint('account', __name__, template_folder='templates')

@account.route('/account')
@login_required
def account_home():
    return render_template('account/account.html', user=current_user)
