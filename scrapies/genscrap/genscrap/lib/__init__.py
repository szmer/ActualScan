import datetime
from datetime import timezone
import http.client
from logging import warning
import time
import urllib
from urllib.parse import urlparse

def date_fmt(time_obj):
    return time_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def timestamp_now():
    return date_fmt(datetime.datetime.now(tz=timezone.utc))

def full_url(part, site_url):
    """
    If the url lacks the domain or scheme part, fill it in using the site url.
    """
    part_parsed = urlparse(part)
    if part_parsed.netloc and part_parsed.scheme:
        return part
    site_url_parsed = urlparse(site_url)
    part_parsed = part_parsed._replace(netloc=site_url_parsed.netloc)
    part_parsed = part_parsed._replace(scheme=site_url_parsed.scheme)
    return part_parsed.geturl()

#
# Page filing.
#
utc_offset_sec = time.altzone if time.localtime().tm_isdst else time.timezone
utc_offset = datetime.timedelta(seconds=-utc_offset_sec)
def now_timestamp():
    return datetime.datetime.now().replace(tzinfo=datetime.timezone(offset=utc_offset)).isoformat()

def write_to_file(path, content):
    with open(path, 'w+') as out_file:
        print(content, file=out_file)

class WebPage():
    def __init__(self, url, text, timestamp, tags, comment):
        self.url = url
        self.text = text
        self.timestamp = timestamp
        self.tags = tags
        self.comment = comment

#
# API communications. 
#
def stractor_reading(text, source_type):
    """
    Get the text of JSON response of Speechtractor for text and source_type, or HTTP status error
    code as int.
    """
    params = urllib.parse.urlencode({'html': text, 'sourcetype': source_type},
            quote_via=urllib.parse.quote)
    headers = {"Content-type": "application/x-www-form-urlencoded",
            "Accept": "text/json"}
    stractor_conn = http.client.HTTPConnection('speechtractor', port=3756, timeout=5)
    stractor_conn.request('POST', '/api/v01/interpret', params, headers)
    stractor_response = stractor_conn.getresponse()
    if stractor_response.status != 200:
        warning(stractor_response.read())
        return stractor_response.status
    return stractor_response.read().decode('utf-8')
