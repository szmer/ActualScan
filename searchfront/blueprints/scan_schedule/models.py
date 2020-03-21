from datetime import datetime, timezone

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
#    scheduled (in Scrapy)
#    Scrapy shouldn't report back against there was a failure, so "ran" doesn't guarantee success.
#    ran
#    failed
#    cancelled

class ScanJob(db.Model):
    # Id should be formed from the user identification and the query (possibly hashed).
    id = db.Column(db.String(512), primary_key=True)
    status = db.Column(db.String(32), nullable=False)
    # The difference is that last checked indicates that there is user interest for the scan job,
    # and the status_changed fields stores when the status actually changed recently.
    last_checked = db.Column(db.DateTime(timezone=True), nullable=False)
    status_changed = db.Column(db.DateTime(timezone=True), nullable=False)
    query_phrase =  db.Column(db.String(512), nullable=False)
    # Just separate tags with commas in the string. It's written and read mostly once, so we don't
    # bother with native Postgres arrays.
    # NOTE we currently *require* tags in scan jobs
    query_tags = db.Column(db.String(8192), nullable=False)
    requests = db.relationship('ScrapeRequest', backref=db.backref('job', lazy=True), lazy=True)
    # This controls whether scrapy should also save raw copies of html to disk. Setting spread to
    # requests.
    save_copies = db.Column(db.Boolean(), nullable=False, default=False)

    def __init__(self):
        self.last_checked = datetime.now(timezone.utc)
        self.status_changed = datetime.now(timezone.utc)
        super().__init__()

    def bump(self):
        self.last_checked = datetime.now(timezone.utc)

    def change_status(self, status):
        self.status = status
        self.last_checked = datetime.now(timezone.utc)
        self.status_changed = datetime.now(timezone.utc)

    @staticmethod
    def identifier(user_id, phrase, tags):
        if isinstance(tags, list):
            tags = ','.join(tags)
        return '{}@@{}@@{}'.format(user_id, phrase, tags)

# TODO address re-crawl/update requests
class ScrapeRequest(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    # Most browsers cut URLs above ~2000 chars, below is the max address bar value for Android.
    url = db.Column(db.String(8192), nullable=False)
    # Tells scrapy to push requests for search results or just save the contents.
    is_search = db.Column(db.Boolean(), nullable=False)
    # Copied from the Site row to avoid the need for joins.
    source_type = db.Column(db.String(32))
    site_name = db.Column(db.String(512), nullable=False)
    site_url = db.Column(db.String(8192), nullable=False)
    query_tags = db.Column(db.String(8192), nullable=False)
    job_id = db.Column(db.String, db.ForeignKey('scan_job.id'), nullable=False)
    # (job field defined as a backref)
    status = db.Column(db.String(32), nullable=False)
    status_changed = db.Column(db.DateTime(timezone=True), nullable=False)
    save_copies = db.Column(db.Boolean(), nullable=False, default=False)
    failure_comment = db.Column(db.String(2048))

    def change_status(self, status):
        self.status = status
        self.status_changed = datetime.now(timezone.utc)
