import pytest

from manager.models import EditSuggestion
from scan.models import ScanJob, Site, Tag

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
        response = client.get('/accounts/login')
        assert response.status_code == 200

    def test_logout(self, client):
        response = client.get('/accounts/logout')
        assert response.status_code == 200

@pytest.mark.django_db
class TestScanViews():
    """
    The views under the scan package.
    """
    def test_account_info(self, client):
        response = client.get('/accounts/info')
        assert response.status_code == 200

    def test_scaninfo(self, client, user):
        response = client.get(reverse('scan:scaninfo'), {'job_id': 0})
        assert response.status_code == 200
        assert 'not found' in response.content
        ScanJob.objects.create(status='terminated', query_phrase='test_progress')
        response = client.get(reverse('scan:scaninfo'), {'job_id': 0})
        assert response.status_code == 200
        assert 'terminated' in response.content

@pytest.mark.django_db
class TestManagerViews():
    """
    The views under the manager package.
    """
    def test_scanlist(self, client, user):
        response = client.get(reverse('manager:scans'))
        assert response.status_code == 200
        assert '<title>Login' in response.content
        client.login(username='test_username', password='password')
        response = client.get(reverse('manager:scans'))
        assert response.status_code == 200
        assert '<title>My scans' in response.content

    def test_sitelist(self, client, user, tag_site_links):
        response = client.get(reverse('manager:sites'))
        assert response.status_code == 200
        assert '<title>Sites' in response.content
        assert 'example.com' in response.content
        assert 'crazy.example.org' in response.content
        response = client.get(reverse('manager:sites_search'), {'q': 'crazy'})
        assert not 'example.com' in response.content
        assert 'crazy.example.org' in response.content

    def test_taglist(self, client, user, tag_site_links):
        response = client.get(reverse('manager:tags'))
        assert response.status_code == 200
        assert '<title>Tags' in response.content
        assert 'test-tag-one' in response.content
        assert 'another-test-tag' in response.content
        response = client.get(reverse('manager:tags_search'), {'q': 'another'})
        assert not 'test-tag-one' in response.content
        assert 'another-test-tag' in response.content

    def test_suggestions(self, client, user, tag_site_links):
        # These should be unaccessible without logging in.
        response = client.get(reverse('manager:suggest'))
        assert response.status_code == 200
        assert '<title>Login' in response.content
        response = client.get(reverse('manager:suggestions'))
        assert response.status_code == 200
        assert '<title>Login' in response.content

        client.login(username='test_username', password='password')
        response = client.get(reverse('manager:suggestions'))
        assert response.status_code == 200
        assert '<title>My suggestions' in response.content

        # Try submitting suggestions.
        assert EditSuggestion.objects.all().count() == 0
        client.login(username='test_username', password='password')
        # (this should be rejected because doesn't change anything:)
        response = client.get(reverse('manager:suggest'),
               {'record_type': 'site', 'target': 'crazy.example.org',
                   'search_pointer': 'http://crazy.example.org/q=twenty+cats',
                   'site_type': 'web', 'source_type': 'blog',
                   'homepage_url': 'http://example.com',
                   'tags': 'another-test-tag'})
        assert EditSuggestion.objects.all().count() == 0
        response = client.get(reverse('manager:suggest'),
               {'record_type': 'site', 'target': 'crazy.example.org',
                   'search_pointer': 'http://crazy.example.org/search?q=twenty+cats',
                   'site_type': 'web', 'source_type': 'blog',
                   'homepage_url': 'http://crazy.example.org',
                   'tags': 'another-test-tag'})
        assert EditSuggestion.objects.all().count() == 1
        response = client.get(reverse('manager:suggest'),
               {'record_type': 'tag', 'target': 'test-tag-one',
                   'description': 'a better, funnier description'})
        assert EditSuggestion.objects.all().count() == 2

    def test_makesite(self, client, user):
        response = client.get(reverse('manager:makesite'))
        assert response.status_code == 200
        assert '<title>Login' in response.content
        client.login(username='test_username', password='password')
        assert Site.objects.all().count() == 0
        response = client.get(reverse('manager:makesite'),
               {'search_pointer': 'http://crazier.example.org/q=twenty+cats',
                   'site_type': 'web', 'source_type': 'forums',
                   'homepage_url': 'http://crazier.example.org',
                   'tags': 'another-test-tag'})
        assert Site.objects.all().count() == 1

    def test_maketag(self, client, user):
        response = client.get(reverse('manager:maketag'))
        assert response.status_code == 200
        assert '<title>Login' in response.content
        client.login(username='test_username', password='password')
        assert Tag.objects.all().count() == 0
        response = client.get(reverse('manager:maketag'),
               {'name': 'new-tag', 'description': 'better than ever before'})
        assert Tag.objects.all().count() == 1
