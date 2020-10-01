from time import sleep

import pytest

from scan.models import Site, ScanJob, ScrapeRequest

@pytest.mark.django_db
class TestScrapping():
    def test_scrapping_process(scrapy_process):
        test_site = Site.objects.create(homepage_url='http://example.com', site_name='example.com',
                search_pointer='http://example.com/?q=twenty+cats',
                site_type='web', source_type='blog')
        test_job = ScanJob.objects.create(user_ip='127.0.0.1', status='working',
                query_phrase='pyramids', website_count=1)
        ScrapeRequest.objects.create(target='http://localhost', is_search=True,
                site=test_site, site_name=test_site.site_name, site_type=test_site.site_type,
                site_url=test_site.homepage_url, job=test_job, status='waiting')
        # Now the request should be picked up by the spider.
        second_request = False
        for try_n in range(20):
            sleep(0.25)
            try:
                second_request = ScrapeRequest.objects.get(is_search=False)
            except ScrapeRequest.DoesNotExist:
                pass
        assert second_request
        assert second_request.target == 'http://localhost/secondpage.html'
        search_request = ScrapeRequest.objects.get(is_search=True)
        assert search_request.status == 'committed'
