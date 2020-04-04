from flask_wtf import FlaskForm
from wtforms import StringField
from wtforms.validators import InputRequired
# wtforms_sqlalchemy is apparently preferred, but it currently breaks the code. The reason seems to
# be related to https://stackoverflow.com/a/48451165 /4.4.2020/
from wtforms.ext.sqlalchemy.fields import QuerySelectMultipleField

from searchfront.blueprints.site import Tag
from searchfront.extensions import db

def tag_choices():
    return db.session.query(Tag).all()

class PublicScanForm(FlaskForm):
    scan_query = StringField('Search for', validators=[InputRequired()])
    query_tags = QuerySelectMultipleField('Scan sites with these tags',
            validators=[InputRequired()],
            query_factory=tag_choices)
