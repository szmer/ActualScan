from logging import debug

from django.contrib.auth.models import User
from django.db import models
from django.utils.timezone import now

SCAN_JOB_STATUSES = [(x, x) for x in [
    'waiting',
    'working',
    'finished',
    'rejected',
    'terminated'
    ]]
SCRAPE_REQUEST_STATUSES = [(x, x) for x in [
    'waiting', # only known, not in the Scrapy queue
    'scheduled',
    'ran',
    'committed', # (two m's, to t's!)
    'failed',
    'cancelled'
    ]]

class Tag(models.Model):
    name = models.CharField(max_length=256, unique=True)
    description = models.CharField(max_length=1024)
    creator = models.ForeignKey(User, on_delete=models.SET_NULL, related_name='tags', null=True)

    def __repr__(self):
        return '/{}/'.format(self.name)

    def __str__(self):
        return '/{}/'.format(self.name)

class Site(models.Model):
    homepage_url = models.CharField(max_length=8192)
    # Homepage url w/o protocol and www, or /r/subreddit
    site_name = models.CharField(max_length=512, unique=True)
    # An url with search for "twenty cats"
    # For Reddit, the subreddit name without /r/
    search_pointer = models.CharField(max_length=8192)
    source_type = models.CharField(max_length=32,
            choices=[(v, v) for v in ['blog', 'forums', 'media', 'social']])
    site_type = models.CharField(max_length=32,
            choices=[(v, v) for v in ['web', 'reddit']])
    creator = models.ForeignKey(User, on_delete=models.SET_NULL, related_name='sites', null=True)

    # NOTE we should take urlencoding schemes into account
    MOCK_STR1 = 'twenty'
    MOCK_STR2 = 'cats'

    def __repr__(self):
        return 'site: {}'.format(self.site_name)

    def __str__(self):
        return 'site: {}'.format(self.site_name)

    def search_url_for(self, tokens):
        """
        An url that should search for tokens on the site.
        """
        mock_idx1 = self.search_pointer.index(self.MOCK_STR1)
        mock_idx2 = self.search_pointer.index(self.MOCK_STR2)
        search_format = (self.search_pointer[:mock_idx1+len(self.MOCK_STR1)]
                + self.search_pointer[mock_idx2+len(self.MOCK_STR2):]).replace(self.MOCK_STR1, '{}')
        next_token_format = self.search_pointer[mock_idx2:mock_idx2+len(self.MOCK_STR2)].replace(
                self.MOCK_STR2, '{}')
        if len(tokens) <= 1:
            # Technically there shouldn't be zero tokens, but we shouldn't crash with this problem
            # now.
            return search_format.format(tokens[0] if tokens else '')
        else:
            for i in range(len(tokens)-1):
                search_format += next_token_format
            return search_format.format(*tokens)

class TagSiteLink(models.Model):
    site = models.ForeignKey(Site, on_delete=models.CASCADE, related_name='tag_links')
    tag = models.ForeignKey(Tag, on_delete=models.CASCADE, related_name='site_links')
    level = models.IntegerField(default=10)

    def __repr__(self):
        return '{}->{}'.format(self.tag.name, self.site.site_name)

    def __str__(self):
        return '{}->{}'.format(self.tag.name, self.site.site_name)

class ScanPermission(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE, null=True, blank=True)
    user_ip = models.CharField(max_length=64, null=True, blank=True)
    time_issued = models.DateTimeField(auto_now_add=True)
    is_used = models.BooleanField(default=False)
    # TODO permission level

class ScanJob(models.Model):
    # (The job id is also used in reason_scraped in Solr.)
    user = models.ForeignKey(User, on_delete=models.CASCADE, null=True, blank=True)
    user_ip = models.CharField(max_length=64, null=True, blank=True)
    status = models.CharField(max_length=32, choices=SCAN_JOB_STATUSES)
    minimal_level = models.IntegerField(default=0)
    # The difference is that last checked indicates that there is user interest for the scan job,
    # and the status_changed fields stores when the status actually changed recently.
    last_checked = models.DateTimeField(auto_now=True)
    status_changed = models.DateTimeField(auto_now_add=True)
    query_phrase = models.CharField(max_length=512)
    # Just separate tags with commas in the string. It's written and read mostly once, so we don't
    # bother with native Postgres arrays.
    # NOTE we currently *require* tags in scan jobs
    query_tags = models.CharField(max_length=8192)
    # These counts should be filled out when starting the job; they're needed for progress reporting
    website_count = models.IntegerField(default=0)
    subreddit_count = models.IntegerField(default=0)
    # This controls whether scrapy should also save raw copies of html to disk. Setting spread to
    # requests.
    save_copies = models.BooleanField(default=False)

    def change_status(self, status):
        self.status = status
        self.status_changed = now()
        self.save()
        debug('Status of the job {} being changed to {}'.format(self.id, status))

    def __repr__(self):
        return '({})job {}/{}, {}'.format(self.pk, self.query_phrase, self.query_tags, self.status)

    def __str__(self):
        return '({})job {}/{}, {}'.format(self.pk, self.query_phrase, self.query_tags, self.status)

class ScrapeRequest(models.Model):
    """
    ScrapeRequest represents one page/URL to be scraped by Scrapy.

    It's also used for requests by auxiliary scrapers, for Reddit etc. Here target is just the
    query phrase verbatim for the search requests, preceded by the appropriate badge in square
    brackets (and a space): [reddit].

    ScrapeRequests with is_search=True are initially created by the start_scan control function.
    """
    # For the site_type==web the target is URL. Otherwise see the class docstring.
    # Most browsers cut URLs above ~2000 chars, below is the max address bar value for Android.
    target = models.CharField(max_length=8192)
    # Tells scrapy to push requests for search results or just save the contents.
    is_search = models.BooleanField()
    # Copied from the Site row to avoid the need for joins.

    source_type = models.CharField(max_length=32)
    site = models.ForeignKey(Site, on_delete=models.CASCADE)
    # Denormalized information from the site. As scrape requests are ephemeral, it's easier to keep
    # it in their rows instead of managing site information in scrapers.
    site_name = models.CharField(max_length=512)
    site_type = models.CharField(max_length=32)
    site_url = models.CharField(max_length=8192)
    query_tags = models.CharField(max_length=8192)
    # This has meaning for search requests. For website requests, this should contain the number of
    # search pages (not implemented). For Reddit requests, this should contain the number total of
    # comments yielded by the search from all submissions in the subreddit.
    lead_count = models.IntegerField(default=0)
    # The job id is also used in reason_scraped in Solr.
    job = models.ForeignKey(ScanJob, related_name='requests', on_delete=models.CASCADE)
    # (job field defined as a backref)
    status = models.CharField(max_length=32, choices=SCRAPE_REQUEST_STATUSES)
    status_changed = models.DateTimeField(auto_now_add=True)
    save_copies = models.BooleanField(default=False)
    failure_comment = models.CharField(max_length=2048)

    def change_status(self, status):
        self.status = status
        self.status_changed = now()
        self.save()
        debug('Status of the scrape request {} being changed to {}'.format(self.id, status))

    def __repr__(self):
        return '({})request {}, {}'.format(self.pk, self.target, self.status)

    def __str__(self):
        return '({})request {}, {}'.format(self.pk, self.target, self.status)

class FeedbackPermission(models.Model):
    """
    A permission to the specific user to give feedback on relevance of the specific site-tag link.
    """
    subject = models.ForeignKey(TagSiteLink, on_delete=models.CASCADE)
    user = models.ForeignKey(User, on_delete=models.CASCADE, null=True, blank=True)
    user_ip = models.CharField(max_length=64, null=True, blank=True)
    time_issued = models.DateTimeField(auto_now_add=True)
    is_used = models.BooleanField(default=False)
