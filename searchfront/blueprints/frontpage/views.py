from flask import Blueprint, render_template

from searchfront.blueprints.frontpage.forms import PublicScanForm

frontpage = Blueprint('frontpage', __name__, template_folder='templates')

@frontpage.route('/')
def home():
    form = PublicScanForm()
    return render_template('frontpage/home.html', form=form)
