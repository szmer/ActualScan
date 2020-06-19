import copy

from flask import g, url_for

from searchfront.extensions import user_datastore
from searchfront.blueprints.site import Site, Tag
from searchfront.test.conftest import TEST_USER_EMAIL, TEST_USER_PASSWORD

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

def clear_test_site(db, test_site_dict):
    existing_sites = list(Site.query.filter_by(**test_site_dict))
    if len(existing_sites) != 0:
        db.session.delete(existing_sites[0])
        db.session.commit()

class TestManagerForms(object):
    def test_enforcing_form_choices(self, app, db, example_user):
        test_tag_dict = {'name': 'test_permissions_tag',
                'description': 'Just a tag for testing permissions.'}
        clear_test_tag(db, test_tag_dict)

        with app.test_request_context():
            test_client = app.test_client()
            with test_client.session_transaction() as sess:
                sess["user_id"] = 1
            # Login.
            resp= test_client.post(url_for('security.login'), data={'email': TEST_USER_EMAIL,
                'password': TEST_USER_PASSWORD}, follow_redirects=True)
            app.logger.debug(
                    'Response when logging in: {}...'.
                    format(format_html_preview(resp.data)))
            #assert 'Redirecting...' in resp.data.decode('utf-8') # means success

            # Submitting a tag we are allowed to create ('community' level). 
            resp = test_client.post(url_for('manager_tag.create_view'),
                    data=dict(list(test_tag_dict.items())
                        + [('csrf_token', g.get('csrf_token'))]))
            app.logger.debug(
                    'Response when trying to create a community tag as a normal user: {}...'.
                    format(format_html_preview(resp.data)))
            existing_tags = list(Tag.query.filter_by(**test_tag_dict))
            assert len(existing_tags) != 0

            clear_test_tag(db, test_tag_dict)
            
            # Trying to submit an elevated tag (not the default 'community' level). 
            elevated_level = 150
            assert elevated_level != 10 # NOTE the current default 'community' level

            elevated_tag_dict = copy.copy(test_tag_dict)
            elevated_tag_dict['level'] = elevated_level
            resp = test_client.post(url_for('manager_tag.create_view'),
                    data=dict(list(elevated_tag_dict.items())
                        + [('csrf_token', g.get('csrf_token'))]))
            app.logger.debug('Response when trying to create a non-community tag as a normal user: '
                    '{}'.format(format_html_preview(resp.data)))

            # As the level is currently excluded from the form, we expect the default value and not
            # the supplied one.
            existing_tags = list(Tag.query.filter_by(**elevated_tag_dict))
            assert len(existing_tags) == 0 or existing_tags[0].level != elevated_level
            clear_test_tag(db, elevated_tag_dict)

    def test_site_creation(self, app, db, example_user):
        test_site_dict = {
                'homepage_url': 'https://example2.com',
                'site_name': 'example2.com',
                'search_pointer': 'https://example2.com/search.php?q=|||fat||| |||cat|||',
                'source_type': 'blog', 'site_type': 'web'
                }
        clear_test_site(db, test_site_dict)

        with app.test_request_context():
            test_client = app.test_client()

            # Login.
            response = test_client.post(url_for('security.login'), data={'email': TEST_USER_EMAIL,
                'password': TEST_USER_PASSWORD})
            assert 'Redirecting...' in response.data.decode('utf-8') # means success

            # Submitting a site. 
            resp = test_client.post(url_for('manager_site.create_view'),
                    data=dict(list(test_site_dict.items())
                        + [('csrf_token', g.get('csrf_token'))]))
            app.logger.debug(
                    'Response when trying to create a site: {}...'.
                    format(format_html_preview(resp.data)))
            existing_sites = list(Site.query.filter_by(**test_site_dict))

            assert len(existing_sites) != 0
            site = existing_sites[0]
            example_user = user_datastore.get_user(TEST_USER_EMAIL) # re-get from db
            print(site.creator_id, example_user.id)
            assert site.creator_id == example_user.id
            clear_test_site(db, test_site_dict)
