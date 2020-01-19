# The MIT License (MIT)
# 
# Copyright (c) 2015 Webhose.io
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#
# originally by Ran Geva
#
# modified by Sz. Rutkowski
#

import re
import json
from dateutil.parser import parse as dateparse
from dateutil.utils import default_tzinfo 
from dateutil.tz import UTC

def parseStrDate(dateString):
    try:
        dateTimeObj = dateparse(dateString)
        return dateTimeObj
    except:
        return None

# Try to extract from the article URL - simple but might work as a fallback
def _extractFromURL(url):

    #Regex by Newspaper3k  - https://github.com/codelucas/newspaper/blob/master/newspaper/urls.py
    m = re.search(r'([\./\-_]{0,1}(19|20)\d{2})[\./\-_]{0,1}(([0-3]{0,1}[0-9][\./\-_])|(\w{3,5}[\./\-_]))([0-3]{0,1}[0-9][\./\-]{0,1})?', url)
    if m:
        return parseStrDate(m.group(0))

    return  None

def _extractFromLDJson(parsedHTML):
    jsonDate = None
    try:
        script = parsedHTML.find('.//script', type='application/ld+json')
        if script is None:
            return None

        data = json.loads(script.text)

        try:
            jsonDate = parseStrDate(data['datePublished'])
        except Exception:
            pass

        try:
            jsonDate = parseStrDate(data['dateCreated'])
        except Exception:
            pass

    except Exception:
        return None

    return jsonDate

def _extractFromMeta(parsedHTML):

    metaDate = None
    for meta in parsedHTML.findall(".//meta"):
        metaName = meta.get('name', '').lower()
        itemProp = meta.get('itemprop', '').lower()
        httpEquiv = meta.get('http-equiv', '').lower()
        metaProperty = meta.get('property', '').lower()

        #<meta name="pubdate" content="2015-11-26T07:11:02Z" >
        if 'pubdate' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name='publishdate' content='201511261006'/>
        if 'publishdate' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name="timestamp"  data-type="date" content="2015-11-25 22:40:25" />
        if 'timestamp' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name="DC.date.issued" content="2015-11-26">
        if 'dc.date.issued' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta property="article:published_time"  content="2015-11-25" />
        if 'article:published_time' == metaProperty:
            metaDate = meta.get('content').strip()
            break
            #<meta name="Date" content="2015-11-26" />
        if 'date' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta property="bt:pubDate" content="2015-11-26T00:10:33+00:00">
        if 'bt:pubdate' == metaProperty:
            metaDate = meta.get('content').strip()
            break
            #<meta name="sailthru.date" content="2015-11-25T19:56:04+0000" />
        if 'sailthru.date' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name="article.published" content="2015-11-26T11:53:00.000Z" />
        if 'article.published' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name="published-date" content="2015-11-26T11:53:00.000Z" />
        if 'published-date' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name="article.created" content="2015-11-26T11:53:00.000Z" />
        if 'article.created' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name="article_date_original" content="Thursday, November 26, 2015,  6:42 AM" />
        if 'article_date_original' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name="cXenseParse:recs:publishtime" content="2015-11-26T14:42Z"/>
        if 'cxenseparse:recs:publishtime' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta name="DATE_PUBLISHED" content="11/24/2015 01:05AM" />
        if 'date_published' == metaName:
            metaDate = meta.get('content').strip()
            break

        #<meta itemprop="datePublished" content="2015-11-26T11:53:00.000Z" />
        if 'datepublished' == itemProp:
            metaDate = meta.get('content').strip()
            break

        #<meta itemprop="datePublished" content="2015-11-26T11:53:00.000Z" />
        if 'datecreated' == itemProp:
            metaDate = meta.get('content').strip()
            break

        #<meta property="og:image" content="http://www.dailytimes.com.pk/digital_images/400/2015-11-26/norway-return-number-of-asylum-seekers-to-pakistan-1448538771-7363.jpg"/>
        if 'og:image' == metaProperty or "image" == itemProp:
            url = meta.get('content').strip()
            possibleDate = _extractFromURL(url)
            if possibleDate is not None:
                return  possibleDate

        #<meta http-equiv="data" content="10:27:15 AM Thursday, November 26, 2015">
        if 'date' == httpEquiv:
            metaDate = meta.get('content').strip()
            break

    if metaDate is not None:
        return parseStrDate(metaDate)

    return None

def _extractFromHTMLTag(parsedHTML):
    #<time>
    for time in parsedHTML.findall(".//time"):
        datetime = time.get('datetime')
        if datetime is not None:
            return parseStrDate(datetime)

        datetime = time.get('class')
        if datetime is not None and datetime[0].lower() == "timestamp":
            return parseStrDate(time.text)

    tag = parsedHTML.find(".//span", {"itemprop": "datePublished"})
    if tag is not None:
        dateText = tag.get("content")
        if dateText is None:
            dateText = tag.text
        if dateText is not None:
            return parseStrDate(dateText)

    #class=
    tagnames = ['span', 'p','div']
    class_markers = ['pubdate', 'timestamp', 'article_date', 'articledate', 'date-pub', 'date_pub']
    for tagname in tagnames:
        for tag in parsedHTML.findall('.//'+tagname):
            if tag.get('class') is None:
                continue
            # Require presence of one of the markers.
            for marker in class_markers:
                if marker in tag.get('class'):
                    break
            else:
                continue

            dateText = tag.text
            if dateText is None:
                dateText = tag.text_content()
            possibleDate = parseStrDate(dateText)

            if possibleDate is not None:
                return possibleDate

    return None

def extractArticlePublishedDate(parsedHTML):
    """The function expects a parsed lxml tree as parsedHTML. Returns a
    datetime.datetime object with intelligently parsed date, or None."""

    articleDate = None

    possibleDate = _extractFromLDJson(parsedHTML)
    if possibleDate is None:
        possibleDate = _extractFromMeta(parsedHTML)
    if possibleDate is None:
        possibleDate = _extractFromHTMLTag(parsedHTML)

    articleDate = possibleDate
    if articleDate is not None:
        articleDate = default_tzinfo(articleDate, UTC)

    return articleDate
