import argparse
from datetime import datetime, timezone
import json
import logging
from logging import getLogger
import os
import random
import re

# Connect to Django.
import sys
sys.path.append('/ascan')
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'ascan.settings')
import django
django.setup()

import praw
from praw.models import Comment, Submission
from praw.exceptions import PRAWException
from prawcore.exceptions import NotFound, ServerError

# Basic NLP.
from sentence_splitter import SentenceSplitter

# Handling Markdown.
import markdown
from markdown_strikethrough.extension import StrikethroughExtension
from mdx_linkify.mdx_linkify import LinkifyExtension
# We eliminate links in a roundabout way, through HTML, and use BeautifulSoup for parsing. The
# performance bottleneck is elsewhere.
from bs4 import BeautifulSoup

from scan.models import ScrapeRequest
from dynamic_preferences.registries import global_preferences_registry

from py_common.apis import solr_check_urls, solr_update
from py_common.time import date_fmt
from py_common.utils import update_request_status
#
# Read command line args.
#
argparser = argparse.ArgumentParser(description=
        'Run the Reddit scraper processing request from Postgres and sending pages to Solr.')
argparser.add_argument('-L', '--loglevel', help='Logging level.')

args = argparser.parse_args()
logger = getLogger()
logging.basicConfig()
if args.loglevel:
    logger.setLevel(args.loglevel)
logger.info('Starting the Reddit scraper.')

#
# Config constants.
#
MARKDOWN_EXTENSIONS = ['fenced_code', 'tables', StrikethroughExtension(), LinkifyExtension()]
SEARCH_LIMIT = 100

SOLR_HOST = os.environ['SOLR_HOST']
SOLR_PORT = os.environ['SOLR_PORT']
SOLR_CORE = os.environ['SOLR_CORE']

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

def make_scrape_request(base_scrape_request, target, job_id, status='ran'):
    logger.debug('Ran a Reddit scrape request for {} ({})'.format(target, job_id))
    return ScrapeRequest.objects.create(target=target,
            is_search=False,
            job_id=base_scrape_request.job_id, status=status,
            status_changed=datetime.now(timezone.utc),
            source_type=base_scrape_request.source_type,
            site_name=base_scrape_request.site_name,
            site_url=base_scrape_request.site_url,
            site_id=base_scrape_request.site_id,
            site_type='reddit',
            save_copies=base_scrape_request.save_copies)

def process_submission(submission, submission_scrape_request):
    logger.info('Processing the submission: {}'.format(submission_scrape_request.target))
    try:
        update_request_status(submission_scrape_request, 'ran')
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
        submission.comments._comments = [comment for comment_n, comment
                in enumerate(submission.comments._comments)
                if not comment_n in deletions]

        # Possibly make the comment for the submission.
        if ok(submission, 'selftext'):
            submission_obj = {'reason_scraped': submission_scrape_request.job_id,
                    'source_type': 'f',
                    'date_retr': date_fmt(datetime.now(tz=timezone.utc)) }
            logger.debug('JSON object for Solr created.')
            submission_obj['text'] = format_text(submission.selftext)
            if ok(submission, 'permalink'):
                submission_obj['url'] = 'https://reddit.com'+submission.permalink
            else:
                update_request_status(submission_scrape_request, 'failed',
                        failure_comment='no permalink')
                return
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
            solr_update(SOLR_HOST, SOLR_PORT, SOLR_CORE, solr_json_text,
                    req_id=submission_scrape_request.id, req_class=ScrapeRequest)

        # Make the comment objects (Solr documents).
        logger.info('Going through the comments...')
        for comment in submission.comments.list():
            # Notify the database.
            comment_scrape_request = make_scrape_request(submission_scrape_request,
                    comment.permalink, submission_scrape_request.job_id)
            process_comment(comment, comment_scrape_request)

        update_request_status(submission_scrape_request, 'committed')
    except (PRAWException, NotFound, ServerError) as e:
        update_request_status(submission_scrape_request, 'failed',
                failure_comment='{}: {}'.format(type(e).__name__, str(e)))

def process_comment(comment, comment_scrape_request):
    try:
        update_request_status(comment_scrape_request, 'ran')
        # Having a permalink is crucial to even indexing the comment.
        if ok(comment, 'permalink'):
            comment_permalink = 'https://reddit.com'+comment.permalink
        else:
            update_request_status(comment_scrape_request, 'failed',
                    failure_comment='no permalink')
            return
        comment_obj = {'reason_scraped': comment_scrape_request.job_id,
                'source_type': 'f',
                'date_retr': date_fmt(datetime.now(tz=timezone.utc)),
                'url': comment_permalink}
        if ok(comment, 'body'):
            comment_obj['text'] = format_text(comment.body)
            if not comment_obj['text']:
                update_request_status(comment_scrape_request, 'failed',
                        failure_comment='no extractable text')
                return
        else:
            update_request_status(comment_scrape_request, 'failed',
                    failure_comment='no text body')
            return
        # TODO ? it would be better to set here the link to whole discussion set on this
        # particular comment
        comment_obj['real_doc'] = 'https://reddit.com'+comment.submission.permalink
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
        solr_update(SOLR_HOST, SOLR_PORT, SOLR_CORE, solr_json_text,
                req_id=comment_scrape_request.id, req_class=ScrapeRequest)
        # Do the manual commit.
        solr_update(SOLR_HOST, SOLR_PORT, SOLR_CORE, '{"commit": {}}',
                req_id=comment_scrape_request.id, req_class=ScrapeRequest)
        update_request_status(comment_scrape_request, 'committed')
    except (PRAWException, NotFound, ServerError) as e:
        update_request_status(comment_scrape_request, 'failed',
                failure_comment='{}: {}'.format(type(e).__name__, str(e)))

#
# Monitor for new search ScrapeRequests and handle them.
#
logger.debug('Reddit scraper loading configuration.')
global_preferences = global_preferences_registry.manager()
REDDIT_SEARCH_DEPTH = global_preferences['scanning__reddit_search_depth']
REDDIT_MANY_COMMENTS_THRESHOLD = global_preferences['scanning__reddit_many_comments_threshold']
REDDIT_MANY_COMMENTS_MINSCORE_RATIO = global_preferences['scanning__reddit_many_comments_minscore_ratio']
DEDUP_DATE_POST_CHECK = global_preferences['scanning__dedup_date_post_check']
DEDUP_DATE_RETR_CHECK = global_preferences['scanning__dedup_date_retr_check']

logger.info('Reddit scraper connecting to Reddit.')
reddit = praw.Reddit(user_agent=os.environ['REDDIT_UA'],
        client_id=os.environ['REDDIT_CLIENT'], client_secret=os.environ['REDDIT_SECRET'])

logger.info('Rerunning the leftover requests.')
leftover_scrape_requests= ScrapeRequest.objects.filter(
    site_type='reddit',
    status__in=['waiting', 'scheduled', 'ran'],
    is_search=False)
for scrape_request in leftover_scrape_requests:
    # Deduplicate with Solr.
    permalink = 'https://reddit.com'+scrape_request.target
    is_url_skippable = solr_check_urls(SOLR_HOST, SOLR_PORT, SOLR_CORE, DEDUP_DATE_POST_CHECK,
            DEDUP_DATE_RETR_CHECK, [permalink])
    if permalink in is_url_skippable:
        update_request_status(scrape_request, 'cancelled', failure_comment='dupe')
        continue

    slash_count = scrape_request.target.count('/')
    if slash_count == 6: # if it's a whole submission, just add it to the queue
        if scrape_request.status != 'waiting':
            update_request_status(scrape_request, 'scheduled')
    elif slash_count == 7: # process the leftover comments now, as we work by submissions
        comment = Comment(reddit, url=permalink)
        process_comment(comment, scrape_request)
# This looks for new awaiting ScrapeRequests in the database, put there by start_scan from
# scan schedule control, from a ScanJob.
logger.info('Reddit scraper running.')
while True:
    # Get new search requests.
    search_scrape_requests_waiting = ScrapeRequest.objects.filter(
        site_type='reddit',
        status='waiting',
        is_search=True)
    #
    # Run the respective searches, effectively for pairs: (subreddit, query).
    #
    for search_scrape_request in search_scrape_requests_waiting:
        logger.debug('Processing the scrape request for {} in {}'.format(
            search_scrape_request.target, search_scrape_request.job_id))
        search_phrase = search_scrape_request.target[len('[reddit] '):]
        try:
            subreddit_name = search_scrape_request.site_name[len('/r/'):]
        except:
            update_request_status(search_scrape_request, 'failed',
                    failure_comment='no /r/ in search_scrape_request.site_name')
            continue
        try:
            update_request_status(search_scrape_request, 'ran')
            subreddit = reddit.subreddit(subreddit_name)

            submissions = subreddit.search('title:{} OR selftext:{}'.format(
                search_phrase, search_phrase), sort='comments', limit=REDDIT_SEARCH_DEPTH)
            # We now need to get the count of the comments, to have an accurate progress estimation.
            # Note that this will load all submission objects, which can take a while.
            submissions_list = []
            submission_requests = dict() # permalink -> request id, for easy retrieval
            for submission in submissions: # is seems that here bad subreddit names may fail
                if not ok(submission, 'permalink'):
                    continue
                is_url_skippable = solr_check_urls(SOLR_HOST, SOLR_PORT, SOLR_CORE,
                        DEDUP_DATE_POST_CHECK, DEDUP_DATE_RETR_CHECK,
                        [submission.permalink])
                if submission.permalink in is_url_skippable:
                    continue
                submission_scrape_request = make_scrape_request(search_scrape_request,
                        submission.permalink, search_scrape_request.job_id, status='scheduled')
                submissions_list.append(submission)
                submission_requests[submission.permalink] = submission_scrape_request.id
        except (PRAWException, NotFound, ServerError) as e:
            update_request_status(search_scrape_request, 'failed',
                    failure_comment='{}: {}'.format(type(e).__name__, str(e)))
        comments_count = sum([subm.num_comments for subm in submissions_list])
        search_scrape_request.lead_count = comments_count
        # TODO REMOVE COMMENT Only now mark the search request as ran - we now know the load of requests it introduces.
        update_request_status(search_scrape_request, 'committed')

    #
    # Pull and run some Reddit submission scheduled for download.
    #
    # Randomize the ORDER BY method, as randomizing the rows themselves could be more costly.
    sorting_methods = ['status_changed', '-status_changed', 'site_name', '-site_name', 'job_id',
            '-job_id']
    submission_scrape_requests = ScrapeRequest.objects.filter(site_type='reddit',
            status='scheduled', is_search=False).order_by(random.choice(sorting_methods))[:1]
    if submission_scrape_requests:
        scrape_request = submission_scrape_requests[0]
        slash_count = scrape_request.target.count('/')
        if slash_count != 6: # it's only a comment, so skip
            continue
        # Do deduplication with Solr.
        permalink = 'https://reddit.com'+scrape_request.target
        is_url_skippable = solr_check_urls(SOLR_HOST, SOLR_PORT, SOLR_CORE,
                DEDUP_DATE_POST_CHECK, DEDUP_DATE_RETR_CHECK,
                [permalink])
        if permalink in is_url_skippable:
            update_request_status(scrape_request, 'cancelled', failure_comment='dupe')
            continue
        # Deduplicate other requests for the same from the same job.
        for other_scrape_request in ScrapeRequest.objects.filter(
                target=scrape_request.target,
                job_id=scrape_request.job_id).exclude(
                        id=scrape_request.id).all():
            update_request_status(other_scrape_request, 'cancelled', failure_comment='dupe')

        submission = Submission(reddit, url=permalink)
        if not ok(submission, 'permalink'):
            update_request_status(scrape_request, 'failed', failure_comment='no permalink')
            continue
        process_submission(submission, scrape_request)

logger.debug('Reddit scraper exited.')
