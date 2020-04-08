from searchfront.lib import now_time
from searchfront.extensions import db

# NOTE we don't use explicit enums here, because we want the easy discoverability of schema on the
# Scrapy side.
#
### Possible scam jobs statuses:
#    waiting
#    working
#    finished
#    rejected
#    terminated
#
### Possible request statuses:
#    waiting
#    scheduled
#    ran
#    committed (two m's, to t's!)
#    failed
#    cancelled

class ScanJob(db.Model):
    # Id should be formed from the user identification and the query (possibly hashed).
    # The job id is also used in reason_scraped in Solr.
    id = db.Column(db.String(512), primary_key=True)
    status = db.Column(db.String(32), nullable=False)
    # The difference is that last checked indicates that there is user interest for the scan job,
    # and the status_changed fields stores when the status actually changed recently.
    last_checked = db.Column(db.DateTime(timezone=True), nullable=False, default=now_time)
    status_changed = db.Column(db.DateTime(timezone=True), nullable=False, default=now_time)
    query_phrase =  db.Column(db.String(512), nullable=False)
    # Just separate tags with commas in the string. It's written and read mostly once, so we don't
    # bother with native Postgres arrays.
    # NOTE we currently *require* tags in scan jobs
    query_tags = db.Column(db.String(8192), nullable=False)
    requests = db.relationship('ScrapeRequest', backref=db.backref('job', lazy=True), lazy=True)
    # This controls whether scrapy should also save raw copies of html to disk. Setting spread to
    # requests.
    save_copies = db.Column(db.Boolean(), nullable=False, default=False)

    def bump(self):
        self.last_checked = now_time()

    def change_status(self, status):
        self.status = status
        self.last_checked = now_time()
        self.status_changed = now_time()

    @staticmethod
    def identifier(user_id, phrase, tags):
        if isinstance(tags, list):
            tags = ','.join(tags)
        return '{}@@{}@@{}'.format(user_id, phrase, tags)

# TODO address re-crawl/update requests
class ScrapeRequest(db.Model):
    """
    ScrapeRequest represents one page/URL to be scraped by Scrapy.

    It's also used for requests by auxiliary scrapers, for Reddit etc. Here target is just the
    query phrase verbatim for the search requests, preceded by the appropriate badge in square
    brackets (and a space): [reddit].

    ScrapeRequests with isSearch=True are initially created by the start_scan control function.
    """
    id = db.Column(db.Integer, primary_key=True)
    # For the site_type==web the target is URL. Otherwise see the class docstring.
    # Most browsers cut URLs above ~2000 chars, below is the max address bar value for Android.
    target = db.Column(db.String(8192), nullable=False)
    # Tells scrapy to push requests for search results or just save the contents.
    is_search = db.Column(db.Boolean(), nullable=False)
    # Copied from the Site row to avoid the need for joins.

    source_type = db.Column(db.String(32))
    site_id = db.Column(db.Integer, db.ForeignKey('site.id'), nullable=False)
    site_name = db.Column(db.String(512), nullable=False)
    site_type = db.Column(db.String(32), nullable=False)
    site_url = db.Column(db.String(8192), nullable=False)
    query_tags = db.Column(db.String(8192), nullable=False)
    # The job id is also used in reason_scraped in Solr.
    job_id = db.Column(db.String, db.ForeignKey('scan_job.id'), nullable=False)
    # (job field defined as a backref)
    status = db.Column(db.String(32), nullable=False)
    status_changed = db.Column(db.DateTime(timezone=True), nullable=False, default=now_time)
    save_copies = db.Column(db.Boolean(), nullable=False, default=False)
    failure_comment = db.Column(db.String(2048))

    def change_status(self, status):
        self.status = status
        self.status_changed = now_time()
