import pytest

from manager.models import EditSuggestion
from scan.models import Site, Tag
from scan.templatetags.scan_extras import phrase_generalizable

from django.urls import reverse

@pytest.mark.django_db
class TestGenericViews():
    """
    The generic and builtin views.
    """
    def test_index(self, client):
        response = client.get('/')
        assert response.status_code == 200

    def test_login(self, client):
        response = client.get('/accounts/login/')
        assert response.status_code == 200

    def test_logout(self, client):
        response = client.get('/accounts/logout/')
        assert response.status_code == 302 # get a redirect

@pytest.mark.django_db
class TestManagerViews():
    """
    The views under the manager package.
    """
    def test_sitelist(self, client, user, tag_site_links):
        response = client.get(reverse('sites'))
        assert response.status_code == 200
        assert 'example.com' in response.content.decode('utf-8')
        assert 'crazy.example.org' in response.content.decode('utf-8')
        response = client.get(reverse('sites_search'), {'q': 'crazy'})
        assert not 'example.com' in response.content.decode('utf-8')
        assert 'crazy.example.org' in response.content.decode('utf-8')

    def test_taglist(self, client, user, tag_site_links):
        response = client.get(reverse('tags'))
        assert response.status_code == 200
        assert 'test-tag-one' in response.content.decode('utf-8')
        assert 'another-test-tag' in response.content.decode('utf-8')
        response = client.get(reverse('tags_search'), {'q': 'another'})
        assert not 'test-tag-one' in response.content.decode('utf-8')
        assert 'another-test-tag' in response.content.decode('utf-8')

    def test_suggestions(self, client, user, tag_site_links):
        # These should be unaccessible without logging in.
        response = client.get(reverse('suggest'))
        assert response.status_code == 302 # get a redirect to the login page
        response = client.get(reverse('suggestions'))
        assert response.status_code == 302 # get a redirect to the login page

        client.login(username='test_username', password='password')
        response = client.get(reverse('suggestions'))
        assert response.status_code == 200

        # Try submitting suggestions.
        assert EditSuggestion.objects.all().count() == 0
        client.login(username='test_username', password='password')
        # (this should be rejected because doesn't change anything:)
        site_id_example = tag_site_links[1].site.id
        response = client.get(reverse('suggest'),
               {'record_type': 'site', 'target_id': site_id_example,
                   'search_pointer': '//crazy.example.org/q=twenty+cats',
                   'site_type': 'web', 'source_type': 'blog',
                   'homepage_url': '//example.com',
                   'tags': ['another-test-tag']})
        assert EditSuggestion.objects.all().count() == 0
        tag = Tag.objects.get(name='another-test-tag')
        response = client.get(reverse('suggest'),
               {'record_type': 'site', 'target_id': site_id_example,
                   'search_pointer': '//crazy.example.org/search?q=twenty+cats',
                   'site_type': 'web', 'source_type': 'blog',
                   'homepage_url': '//crazy.example.org',
                   'tags': [tag.id]})
        assert EditSuggestion.objects.all().count() == 1
        tag_id_example = tag_site_links[1].tag.id
        response = client.get(reverse('suggest'),
               {'record_type': 'tag', 'target_id': tag_id_example,
                   'description': 'a better, funnier description'})
        assert EditSuggestion.objects.all().count() == 2

    def test_makesite(self, client, user, tag_site_links):
        response = client.get(reverse('makesite'))
        assert response.status_code == 302 # get a redirect to the login page
        client.login(username='test_username', password='password')
        assert Site.objects.all().count() == 2 # from the fixture
        tag = Tag.objects.get(name='another-test-tag')
        response = client.post(reverse('makesite'),
               {'search_pointer': '//crazier.example.org/q=twenty+cats',
                   'site_type': 'web', 'source_type': 'forums',
                   'homepage_url': '//crazier.example.org',
                   'tags': [tag.id]})
        assert Site.objects.all().count() == 3

    def test_maketag(self, client, user):
        response = client.get(reverse('maketag'))
        assert response.status_code == 302 # get a redirect to the login page
        client.login(username='test_username', password='password')
        assert Tag.objects.all().count() == 0
        response = client.post(reverse('maketag'),
               {'name': 'new-tag', 'description': 'better than ever before'})
        assert Tag.objects.all().count() == 1

    def test_tag_phrase_generalizable(self):
        assert not phrase_generalizable('cats')
        assert not phrase_generalizable('cats ')
        assert phrase_generalizable('twenty cats')
