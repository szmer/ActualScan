from flask_wtf import FlaskForm
from wtforms import StringField, HiddenField
from wtforms.validators import InputRequired, Length, ValidationError

from searchfront.blueprints.site import Site, Tag

class ChangeRequestForm(FlaskForm):
    subject = HiddenField()
    content = StringField('Your suggestion', validators=[InputRequired(), Length(min=3, max=5000)])

    error_msg = 'A bad change request field, please report this to the admins. Thank you!'

    def extract_subject_id(self, subject_text):
        if subject_text is None or not subject_text:
            return None, None

        id_pos = -1
        subject_type = None
        if 'site: ' == subject_text[:len('site: ')]:
            id_pos = len('site: ')
            subject_type = 'site'
        if 'tag: ' == subject_text[:len('tag: ')]:
            id_pos = len('tag: ')
            subject_type = 'tag'

        if id_pos == -1:
            raise ValidationError(self.error_msg)

        id = subject_text[id_pos:]
        return subject_type, id

    def validate_subject(self, field):
        subject_type, id = self.extract_subject_id(field.data)
        if subject_type == 'site' and  Site.query.get(id) is None:
            raise ValidationError(self.error_msg)
        if subject_type == 'tag' and  Tag.query.get(id) is None:
            raise ValidationError(self.error_msg)
