import argparse
from datetime import datetime, timezone
import json
from logging import debug, getLogger, info
import re

import praw
from sqlalchemy.ext.automap import automap_base
from sqlalchemy.orm import Session
from sqlalchemy import create_engine

# Basic NLP.
from sentence_splitter import SentenceSplitter

# Handling Markdown.
import markdown
from markdown_strikethrough.extension import StrikethroughExtension
from mdx_linkify.mdx_linkify import LinkifyExtension
# We eliminate links in a roundabout way, through HTML, and use BeautifulSoup for parsing. The
# performance bottleneck is elsewhere.
from bs4 import BeautifulSoup

# We need to get one module deeper comparing to the general spider, because we're outside of the
# Scrapy project.
from genscrap.genscrap.lib import date_fmt, solr_update, site_id_tags, update_request_status
from genscrap.genscrap.flask_instance.settings import (
        SQLALCHEMY_DATABASE_URI, REDDIT_UA, REDDIT_CLIENT, REDDIT_SECRET
        )
#
# Read command line args.
#
argparser = argparse.ArgumentParser(description=
        'Run the Reddit scraper processing request from Postgres and sending pages to Solr.')
argparser.add_argument('-L', '--loglevel', help='Logging level.')

args = argparser.parse_args()
if args.loglevel:
    logger = getLogger()
    logger.setLevel(args.loglevel)

#
# Config constants.
#
MARKDOWN_EXTENSIONS = ['fenced_code', 'tables', StrikethroughExtension(), LinkifyExtension()]
SEARCH_LIMIT = 100

#
# Postgres connection & ORM setup.
#
AutomapBase = automap_base()
pg_engine = create_engine(SQLALCHEMY_DATABASE_URI)
# Reflect the tables.
AutomapBase.prepare(pg_engine, reflect=True)

ScrapeRequest = AutomapBase.classes.scrape_request
LiveConfigValue = AutomapBase.classes.live_config_value
Site = AutomapBase.classes.site
pg_session = Session(pg_engine)

#
# Utility functions.
#
def ok(obj, attrname):
    """
    A convenience function to check if the object attribute exists, is not None and not False.
    """
    return hasattr(obj, attrname) and getattr(obj, attrname) is not None and getattr(obj, attrname)

splitter = SentenceSplitter(language='en')
# TODO ignore "[deleted]" comments
def format_text(text):
    """
    This can return False if the text is reject (if short and not alphanumeric).
    """
    # Remove comments with little alphabetic content.
    if (len(text) < 50 and len(re.sub('[^\\W0-9]', '', text)) / len(text) >= 0.65):
        return False
    # Strip Markdown markup through HTML. Not efficient, but API quota is the bottleneck.
    # TODO ? replace links
    html = markdown.markdown(text, extensions=MARKDOWN_EXTENSIONS)
    text = BeautifulSoup(html, 'html.parser').text
    # We want paragraphs separated by two new lines, and sentences by one.
    paragraphs = text.split('\n\n')
    paragraphs = [splitter.split(par) for par in paragraphs]
    first_par = True
    result = ''
    for par in paragraphs:
        if first_par:
            first_par = False
        else:
            result += '\n'
        for sent in par:
            result += sent + '\n'
    return result

def make_scrape_request(target, job_id):
    info('Ran a Reddit scrape request for {} ({})'.format(target, job_id))
    return ScrapeRequest(target=target,
            is_search=False,
            job_id=search_scrape_request.job_id, status='ran',
            status_changed=datetime.now(timezone.utc),
            source_type=search_scrape_request.source_type,
            query_tags=search_scrape_request.query_tags,
            site_name=search_scrape_request.site_name,
            site_url=search_scrape_request.site_url,
            site_id=search_scrape_request.site_id,
            site_type='reddit',
            save_copies=search_scrape_request.save_copies)

#
# Monitor for new search ScrapeRequests and handle them.
#
debug('Reddit scraper loading configuration.')
REDDIT_SEARCH_DEPTH = int(pg_session.query(LiveConfigValue).get('reddit_search_depth').value)
REDDIT_MANY_COMMENTS_THRESHOLD = int(pg_session.query(LiveConfigValue).get(
    'reddit_many_comments_threshold').value)
REDDIT_MANY_COMMENTS_MINSCORE_RATIO = int(pg_session.query(LiveConfigValue).get(
    'reddit_many_comments_minscore_ratio').value)

debug('Reddit scraper connecting to Reddit.')
reddit = praw.Reddit(user_agent=REDDIT_UA, client_id=REDDIT_CLIENT, client_secret=REDDIT_SECRET)
# This looks for new awaiting ScrapeRequests in the database, put there by start_scan from
# scan schedule control, from a ScanJob.
debug('Reddit scraper running.')
while True:
    # Get new search requests.
    search_scrape_requests_waiting = list(pg_session.query(ScrapeRequest).filter(
        ScrapeRequest.site_type == 'reddit',
        ScrapeRequest.status == 'waiting',
        ScrapeRequest.is_search == True))
    # Run the respective searches, effectively for pairs: (subreddit, query).
    for search_scrape_request in search_scrape_requests_waiting:
        debug('Processing the scrape request for {} in {}'.format(
            search_scrape_request.target, search_scrape_request.job_id))
        update_request_status(pg_session, search_scrape_request, 'ran')
        # Scan job level submission deduplication. TODO just consult with Solr!
        downloaded_submissions = set()
        search_phrase = search_scrape_request.target[len('[reddit] '):]
        try:
            subreddit_name = search_scrape_request.site_name[len('/r/'):]
        except:
            continue
        subreddit = reddit.subreddit(subreddit_name)
        subreddit_tags = site_id_tags(search_scrape_request.site_id, pg_session)
        submissions = subreddit.search('title:{} OR selftext:{}'.format(
            search_phrase, search_phrase), sort='comments', limit=REDDIT_SEARCH_DEPTH)
        # Process submissions from this subreddit.
        for submission in submissions:
            # Submission deduplication (limited to the ScanJob).
            if not ok(submission, 'permalink'):
                continue
            if submission.permalink in downloaded_submissions:
                continue
            # Add to the dedup and to the database.
            downloaded_submissions.add(submission.permalink)
            submission_scrape_request = make_scrape_request(submission.permalink,
                    search_scrape_request.job_id)
            pg_session.add(submission_scrape_request)
            pg_session.commit()
            # Load all the 'load more's.
            submission.comments.replace_more(limit=None)
            # Apply some rules for deleting comments on the top level.
            minimum_score = 0
            if len(submission.comments) >= REDDIT_MANY_COMMENTS_THRESHOLD:
                minimum_score = int(len(submission.comments) / REDDIT_MANY_COMMENTS_MINSCORE_RATIO)
            # NOTE this relies on existing the _comments field in CommentsForest object as a list.
            deletions = []
            for comment_n, comment in enumerate(submission.comments._comments):
                # Remove comments downvoted into oblivion.
                if comment.score < minimum_score:
                    deletions.append(comment_n)
                # Remove comments with little alphabetic content.
                # NOTE Currently we're just storing everything.
###-                if (not ok(comment, 'body') or (len(comment.body) < 50
###-                        and len(re.sub('[^\\W0-9]', '', comment.body)) / len(comment.body) >= 0.65)):
###-                    deletions.append(comment_n)
            submission.comments._comments = [comment for comment_n, comment
                    in enumerate(submission.comments._comments)
                    if not comment_n in deletions]
            # Make the submission's object (Solr document).
            if ok(submission, 'selftext'):
                submission_obj = {'reason_scraped': search_scrape_request.job_id, 'source_type': 'f',
                        'date_retr': date_fmt(datetime.now(tz=timezone.utc)),
                        'tags': subreddit_tags }
                submission_obj['text'] = format_text(submission.selftext)
                if ok(submission, 'permalink'):
                    submission_obj['url'] = 'https://reddit.com'+submission.permalink
                else:
                    continue
                submission_obj['real_doc'] = 'self'
                if ok(submission, 'author'):
                    submission_obj['author'] = submission.author.name
                if ok(submission, 'created_utc'):
                    time_obj = datetime.fromtimestamp(submission.created_utc)
                    submission_obj['date_post'] = date_fmt(time_obj)
                if ok(submission, 'subreddit'):
                    if ok(submission.subreddit, 'display_name'):
                        submission_obj['site_name'] = '/r/' + submission.subreddit.display_name
                    if ok(submission.subreddit, 'over_18') and submission.subreddit.over_18:
                        submission_obj['adult_b'] = True
                # Send to Solr.
                solr_json_text = json.dumps([submission_obj])
                solr_update(solr_json_text, req_id=submission_scrape_request.id,
                        req_class=ScrapeRequest, pg_session=pg_session)
            # Make the comment's object (Solr documents).
            for comment in submission.comments.list():
                if ok(comment, 'permalink'):
                    comment_permalink = 'https://reddit.com'+comment.permalink
                else:
                    continue
                # Notify the database.
                downloaded_submissions.add(submission.permalink)
                submission_scrape_request = make_scrape_request(submission.permalink,
                    search_scrape_request.job_id)
                pg_session.add(submission_scrape_request)
                pg_session.commit()
                comment_obj = {'reason_scraped': search_scrape_request.job_id, 'source_type': 'f',
                        'date_retr': date_fmt(datetime.now(tz=timezone.utc)),
                        'url': comment_permalink, 'tags': subreddit_tags }
                if ok(comment, 'body'):
                    comment_obj['text'] = format_text(comment.body)
                    if not comment_obj['text']:
                        continue
                else:
                    continue
                # TODO ? it would be better to set here the link to whole discussion set on this comment
                if ok(submission, 'permalink'):
                    comment_obj['real_doc'] = 'https://reddit.com'+submission.permalink
                else:
                    continue
                if ok(comment, 'author'):
                    comment_obj['author'] = comment.author.name
                if ok(comment, 'created_utc'):
                    time_obj = datetime.fromtimestamp(comment.created_utc)
                    comment_obj['date_post'] = date_fmt(time_obj)
                if ok(comment, 'subreddit'):
                    if ok(comment.subreddit, 'display_name'):
                        comment_obj['site_name'] = '/r/' + comment.subreddit.display_name
                    if ok(comment.subreddit, 'over_18') and comment.subreddit.over_18:
                        comment_obj['adult_b'] = True
                # Send to Solr (this needs a manual commit).
                solr_json_text = json.dumps([comment_obj])
                solr_update(solr_json_text, req_id=submission_scrape_request.id,
                        req_class=ScrapeRequest, pg_session=pg_session)
                # Do the manual commit.
                solr_update('{"commit": {}}', req_id=submission_scrape_request.id,
                        req_class=ScrapeRequest, pg_session=pg_session)
debug('Reddit scraper exited.')
