from flask_wtf import FlaskForm
from wtforms import StringField
from wtforms.validators import DataRequired
from wtforms.ext.sqlalchemy.fields import QuerySelectMultipleField

from searchfront.blueprints.site import Tag
from searchfront.extensions import db

def tag_choices():
    return db.session.query(Tag).all()

class PublicScanForm(FlaskForm):
    scan_query = StringField('Search for', validators=[DataRequired()])
    query_tags = QuerySelectMultipleField('Scan sites with these tags',
            validators=[DataRequired()],
            query_factory=tag_choices)
