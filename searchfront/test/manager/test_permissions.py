import copy

from flask import g, url_for

from searchfront.blueprints.site import Tag

def format_html_preview(text):
    text = str(text)
    form_pos = text.find('form')
    if form_pos != -1:
        return text[form_pos:form_pos+3000]
    else:
        container_pos = text.find('container')
        if container_pos != -1:
            return text[container_pos:container_pos+3000]
        else:
            return text[:3000]

def clear_test_tag(db, test_tag_dict):
    existing_tags = list(Tag.query.filter_by(**test_tag_dict))
    if len(existing_tags) != 0:
        db.session.delete(existing_tags[0])
        db.session.commit()

class TestManagerPermissions(object):
    def test_enforcing_form_choices(self, app, db, example_user):
        test_tag_dict = {'name': 'test_permissions_tag', 'level': 'community',
                'description': 'Just a tag for testing permissions.'}
        clear_test_tag(db, test_tag_dict)

        with app.test_request_context():
            test_client = app.test_client()

            # Login.
            test_client.post(url_for('security.login'), data={'email': 'test@example.com',
                'password': 'password'})

            # Submitting a tag we are allowed to create ('community' level). 
            resp = test_client.post(url_for('manager_tag_registered.create_view'),
                    data=dict(list(test_tag_dict.items())
                        + [('csrf_token', g.get('csrf_token'))]))
            app.logger.debug(
                    'Response when trying to create a community tag as a normal user: {}...'.
                    format(format_html_preview(resp.data)))
            existing_tags = list(Tag.query.filter_by(**test_tag_dict))
            assert len(existing_tags) != 0

            clear_test_tag(db, test_tag_dict)
            
            # Trying to submit an elevated tag ('base' instead of 'community' level). 
            elevated_tag_dict = copy.copy(test_tag_dict)
            elevated_tag_dict['level'] = 'base'
            resp = test_client.post(url_for('manager_tag_registered.create_view'),
                    data=dict(list(elevated_tag_dict.items())
                        + [('csrf_token', g.get('csrf_token'))]))
            app.logger.debug('Response when trying to create a base tag as a normal user: {}'.
                    format(format_html_preview(resp.data)))
            try:
                assert 'Not a valid choice' in str(resp.data)
                existing_tags = list(Tag.query.filter_by(**elevated_tag_dict))
                assert len(existing_tags) == 0
            except AssertionError:
                clear_test_tag(db, elevated_tag_dict)
