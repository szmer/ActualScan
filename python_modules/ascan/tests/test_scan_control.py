import pytest

from scan.control import (
        maybe_give_feedback_tag_site_link, maybe_issue_guest_scan_permission, request_scan,
        scan_progress_info, spare_scan_capacity, start_scan, terminate_scan, verify_scan_permission
        )
from scan.models import (
        ScanJob, ScanPermission, ScrapeRequest, Site, TagSiteLink
        )

@pytest.mark.django_db
class TestScanControl():
    def test_scan_start_stop(self, tag_site_links):
        test_scan_capacity1 = spare_scan_capacity()

        request_scan('127.0.0.1', 'hey',
                query_tags=['test-tag-one'], is_ip=True)
        test_scan_job = ScanJob.objects.get(query_phrase='hey')
        assert test_scan_job
        # Check if marking the identification as IP works.
        test_scan_job = ScanJob.objects.get(user_ip='127.0.0.1')
        assert test_scan_job

        # The waiting job should change the scan capacity.
        test_scan_capacity2 = spare_scan_capacity()
        assert (test_scan_capacity1 - test_scan_capacity2 == 1)

        start_scan(test_scan_job)
        requests = ScrapeRequest.objects.filter(job=test_scan_job)
        assert requests
        requests = ScrapeRequest.objects.filter(job=test_scan_job, status='waiting')
        assert requests

        terminate_scan(test_scan_job.id)
        requests = ScrapeRequest.objects.filter(job=test_scan_job, status='waiting')
        assert not requests
        requests = ScrapeRequest.objects.filter(job=test_scan_job, status='cancelled')
        assert requests
        test_scan_job = ScanJob.objects.get(query_phrase='hey', status='terminated')
        assert test_scan_job

    def test_verify_scan_permission(self, user):
        # KLUDGE currently always issuing for registered
        assert verify_scan_permission(user, '127.0.0.1')
        # TODO! get an unauthorized user for testing, currently can't figure out how to do this

    def test_feedback_permission(self, user, tag_site_links):
        test_link = TagSiteLink.objects.get(tag__name='another-test-tag')
        test_permission = maybe_give_feedback_tag_site_link(user, [test_link])
        assert isinstance(test_permission, TagSiteLink)
        test_permission = maybe_give_feedback_tag_site_link("127.0.0.1", [test_link], is_ip=True)
        assert isinstance(test_permission, TagSiteLink)

    def test_guest_scan_permission(self):
        is_issued = maybe_issue_guest_scan_permission("127.0.0.1")
        assert is_issued
        assert ScanPermission.objects.all().count() == 1
        # Forbid duplicate unused permissions.
        is_issued = maybe_issue_guest_scan_permission("127.0.0.1")
        assert is_issued
        assert ScanPermission.objects.all().count() == 1

    def test_scan_progress_info(self, tag_site_links):
        test_job = ScanJob.objects.create(status='waiting', query_phrase='test_progress',
                website_count=1)
        progress = scan_progress_info(test_job.id)
        assert progress
        assert 'phase' in progress
        assert progress['phase'] == 'waiting'
        assert 'dl_proportion' in progress
        assert float(progress['dl_proportion']) == 0.0

        test_job.change_status('working')
        progress = scan_progress_info(test_job.id)
        assert 'phase' in progress
        assert progress['phase'] == 'working'
        assert 'dl_proportion' in progress
        assert float(progress['dl_proportion']) == 0.0

        # Make sure that the estimated finished proportion only goes up.
        proportions = []

        test_site = Site.objects.get(site_name='example.com')
        test_req1 = ScrapeRequest.objects.create(target='dummy.target.com/search', is_search=True,
                source_type='blog', site=test_site, site_type='web', site_url='example.com',
                status='waiting', job=test_job)
        progress = scan_progress_info(test_job.id)
        assert 'phase' in progress
        assert progress['phase'] == 'working'
        assert 'dl_proportion' in progress
        assert float(progress['dl_proportion']) == 0.0
        proportions.append(progress['dl_proportion'])

        test_req1.change_status('committed')
        test_req2 = ScrapeRequest.objects.create(target='dummy.target.com/page1', is_search=False,
                source_type='blog', site=test_site, site_type='web', site_url='example.com',
                status='waiting', job=test_job)
        test_req3 = ScrapeRequest.objects.create(target='dummy.target.com/page2', is_search=False,
                source_type='blog', site=test_site, site_type='web', site_url='example.com',
                status='waiting', job=test_job)
        progress = scan_progress_info(test_job.id)
        assert 'dl_proportion' in progress
        assert float(progress['dl_proportion']) > 0.0
        assert float(progress['dl_proportion']) < 1.0
        proportions.append(progress['dl_proportion'])

        # "Simulate" gradually committing the non-search requests.
        test_req2.change_status('committed')
        progress = scan_progress_info(test_job.id)
        assert 'dl_proportion' in progress
        assert float(progress['dl_proportion']) > 0.0
        assert float(progress['dl_proportion']) < 1.0
        proportions.append(progress['dl_proportion'])

        test_req3.change_status('committed')
        progress = scan_progress_info(test_job.id)
        assert 'dl_proportion' in progress
        assert float(progress['dl_proportion']) == 1.0
        proportions.append(progress['dl_proportion'])

        # The reported proportion should never decrease.
        assert all(x<=y for x, y in zip(proportions, proportions[1:]))
