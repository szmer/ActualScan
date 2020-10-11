import pytest

from django.contrib.auth.models import User

from scan.models import Site, Tag, TagSiteLink

@pytest.fixture
def user(db):
    example_user = User.objects.create_user(username='test_username',
            email='someone@example.com', password='password')
    return example_user

@pytest.fixture
def tag_site_links(db):
    test_tag1 = Tag.objects.create(name='test-tag-one', description='a tag for testing')
    test_tag2 = Tag.objects.create(name='another-test-tag', description='another tag for testing')
    test_site1 = Site.objects.create(homepage_url='http://example.com', site_name='example.com',
            search_pointer='http://example.com/?q=twenty+cats',
            site_type='web', source_type='blog')
    test_site2 = Site.objects.create(homepage_url='http://crazy.example.org',
            site_name='crazy.example.org', search_pointer='http://crazy.example.org/?q=twenty+cats',
            site_type='web', source_type='media')
    links = []
    links.append(TagSiteLink.objects.create(site=test_site1, tag=test_tag1))
    links.append(TagSiteLink.objects.create(site=test_site2, tag=test_tag1))
    links.append(TagSiteLink.objects.create(site=test_site2, tag=test_tag2))
    return links
