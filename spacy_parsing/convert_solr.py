import os
import re
from datetime import timezone
from urllib.parse import urlparse
import xml.etree.cElementTree as ET
from sentence_splitter import SentenceSplitter
from lxml import html
from jusText_star.justext.core import justext
# WARNING this uses the original installed justext package directory!
from jusText_star.justext.utils import get_stoplist

def date_fmt(time_obj):
    return time_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def paragraph_report(par):
    print('~~~')
    print('Paragraph content: {}'.format(par.text[:180]))
    print('Paragraph length: {}'.format(len(par.text)))
    print('Paragraph classification: {}'.format(par.class_type))
    print('Paragraph stopword density: {}'.format(par.stopwords_density(get_stoplist('English'))))
    print('Paragraph links density: {}'.format(par.links_density()))
    print('~~~')

#
# CONFIG desired range of rows here.
#
#ids = [1970]
ids = range(2240, 2250)#2326)
verbose_ids = [2249]

VERBOSE = False
# This regexes are searched for when in VERBOSE mode and paragraphs with them get shown some diagnostics.
WATCH_PATTERNS = ['autopilot', 'Welcome']

site_types = {
        'head-fi': 'f',
        'forums': 'f',
        'cnet': 'm',
        'dazeddigital': 'm',
        'fashionista': 'm',
        'glamour': 'm',
        'majorhifi': 'm',
        'leadsrating': 'm',
        'thefashionables': 'm',
        'thefashionpolice': 'm',
        'vogue': 'm',
        'wwd': 'm',
        'askandyaboutclothes': 'f',
        'edcforums': 'f',
        'reddit': 'f',
        'redflagdeals': 'f',
        'styleforum': 'f',
        'thefashionspot': 'f',
        'thestudentroom': 'f',
        'wallstreetoasis': 'f',
        'youlookfab': 'f', # NOTE we look at domain, but really its the /welookfab directory!
        'dieworkwear': 'b',
        'effortlesseverydaystyle': 'b',
        'fashionbyina': 'b',
        'frugalfashionshopper': 'b',
        'homestudio': 'b',
        'headphonedungeon': 'b',
        'jseverydayfashion': 'b',
        'kendieveryday': 'b',
        'pennypincherfashion': 'b',
        'soundgearlab': 'b',
        'themodestman': 'b',
        }

output_filename = 'test_solr.xml'

splitter = SentenceSplitter(language='en')

#
# Site-specific helpers.
#

def is_newdoc(tag, attrs):
    result = False
    if (None, 'class') in attrs: # None namespace
        # \b "matches the empty string, but only at the beginning or end of a word."
        # on head-fi, stevehuffman, audioholics (Xenforo software)
        # also thefashionspot
        result = re.search('\\bmessage\\b', attrs.getValue((None, 'class')))
        # on redflagdeals
        result = result or re.search('\\bthread_post\\b', attrs.getValue((None, 'class')))
        # on welookfab (but not the first post)
        result = result or re.search('\\breply\\b', attrs.getValue((None, 'class')))
    return result

def doc_text_transform_xenforo1(text_sections):
    """Text_sections are not sentence-splitted."""
    result_sections = []
    metadata = dict()
    for sec in text_sections:
        # Skip the loose metadata.
        if (len(sec) < 120 and
                (sec.strip() in ['Click to expand...', 'Headphoneus Supremus']
                or True in [bool(re.search(p, sec.replace('\n', ' ')))
                    for p in ['^((1|5)000?\\+ )?(New )?Head-Fier$',
                        '^(Likes|Posts): [0-9,]+$',
                        '^Joined: \\w{3} [0-9]{2}, [0-9]{4}$',
                        '^[0-9,]+$']])):
            continue
        result_sections.append(sec)
    return result_sections, metadata


# NOTE WHEN SCRAPING WE WANT TO HAVE UTC AS OUR LOCAL TIMEZONE!

#
# Getting and filing documents.
#
xml = ET.Element('add')
processed_urls = set()
all_elems = [] # keep them for the ET to write
with open(output_filename, 'w+') as output_file:
    for page_id in ids:

        #
        # Getting additional data from the filesystem.
        #
        if page_id in verbose_ids:
            VERBOSE = True
        elif verbose_ids:
            VERBOSE = False

        if VERBOSE:
            print('---------')
            print('Processing id {}'.format(page_id))
            print('---------')
        # (skip files marked as dead - ie. duplicates)
        if os.path.isfile('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/dead'):
            if VERBOSE:
                print('The file is dead, skipping.')
            continue
        if not os.path.isfile('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/index'):
            if VERBOSE:
                print('The file has no index html downloaded, skipping.')
            continue
        with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/access-timestamp') as access_file:
            date_retrieved = access_file.read().strip()
            if VERBOSE:
                print('The retrieved access date is {}, formatting for Solr.'.format(date_retrieved))
            # Don't bother with properly formatting the original hour etc. part.
            date_retrieved = date_retrieved[:date_retrieved.index('T')] + 'T00:00:00Z'
            
        with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/url') as url_file:
            url = url_file.read().strip()
            if VERBOSE:
                print('The retrieved URL is {}.'.format(url))

        # Try to determine the source type with the url.
        parsed_url = urlparse(url)
        for (string, stype) in site_types.items():
            if string in parsed_url.netloc: # search in the domain name
                source_type = stype
                if VERBOSE:
                    print('Determined source_type: {}.'.format(source_type))
                break
        else:
            if VERBOSE:
                print('Cannot determine source type, skipping.')
            continue

        #
        # Process the HTML index.
        #
        with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/index') as html_file:

            multidocs = (source_type == 'f') # true on forums with many posts on one page
            if VERBOSE:
                if multidocs:
                    print('Entering multidocs mode.')
                else:
                    print('One doc per page mode.')

            #
            # Collect the documents list of (title, list of sections as strings)
            dom = html.fromstring(str.encode(html_file.read()))

            # Extract title (Xpath expression for findding anywhere in the document)
            doc_title = dom.findtext('.//title')
            if VERBOSE:
                print('HTML page title: {}.'.format(doc_title))

            if multidocs:
                paragraphs = justext(dom, get_stoplist('English'), docstart_fun=is_newdoc,
                        # relaxed parameters for forums.
                        stopwords_low=0.17, stopwords_high=0.22, length_high=110,
                        # but at least be more stringent about link density
                        max_link_density=0.16)
            else:
                paragraphs = justext(dom, get_stoplist('English'))

            if VERBOSE:
                print('{} paragraphs retrieved ({} good, {} separate documents).'.format(
                    len(paragraphs),
                    len([par for par in paragraphs if par.class_type == 'good']),
                    len([par for par in paragraphs if par.docstart])))

            if multidocs:
                documents = []
                current_sections = [] # sections are paragraphs
                pre_doc = True # before the first document/post
                metadata = dict()

                for par in paragraphs:
                    if par.docstart: # commit the current post
                        metadata = par.doc_metadata
                        if VERBOSE:
                            print('_docstart_: "{}"'.format(re.sub('\\s', ' ', par.text[:40])))
                        if pre_doc:
                            pre_doc = False
                        else:
                            #current_sections, extra_meta = doc_text_transform_xenforo1(current_sections)
                            documents.append((doc_title, current_sections, metadata))
                            current_sections = []

                    # Save good paragraphs.
                    if par.class_type == 'good' and not pre_doc:
                        current_sections, extra_meta = doc_text_transform_xenforo1(current_sections)
                        current_sections.append(par.text)

                    if VERBOSE:
                        for pattern in WATCH_PATTERNS:
                            if re.search(pattern, par.text):
                                paragraph_report(par)

                # Commit the last post if needed.
                if len(current_sections) != 0:
                    current_sections, extra_meta = doc_text_transform_xenforo1(current_sections)
                    documents.append((doc_title, current_sections, metadata))

            if not multidocs:
                sections = [par.text for par in paragraphs if par.class_type == 'good']
                documents = [(doc_title, sections, paragraphs[0].doc_metadata)]

                if VERBOSE and WATCH_PATTERNS:
                    for pattern in WATCH_PATTERNS:
                        if re.search(pattern, par.text):
                            paragraph_report(par)

            #
            # Write the documents to XML.
            skipped_no_permalink = 0
            for title, sections, metadata in documents:

                # Skip forum posts with no permalink.
                if multidocs and not 'permalink' in metadata:
                    skipped_no_permalink += 1
                    continue

                text = ''
                for sec_str in sections: # section contains one paragraph text
                    sec_sents = splitter.split(sec_str)
                    # Each sentence is delimited with \n, and each section with two \n's
                    for sent in sec_sents:
                        text += sent + '\n'
                    text += '\n'
                text = text.strip()
                # Skip if text would be empty.
                if len(text) == 0:
                    continue

                doc_elem = ET.SubElement(xml, 'doc')
                all_elems.append(doc_elem)

                title_elem = ET.SubElement(doc_elem, 'field', {'name': 'title'})
                title_elem.text = title
                url_elem = ET.SubElement(doc_elem, 'field', {'name': 'url'})
                if multidocs:
                    permalink = metadata['permalink']
                    parsed_permalink = urlparse(permalink)
                    if not parsed_permalink.netloc: # add domain if needed
                        parsed_permalink = parsed_permalink._replace(netloc=parsed_url.netloc)
                        parsed_permalink = parsed_permalink._replace(scheme=parsed_url.scheme)
                    url_elem.text = parsed_permalink.geturl()
                else:
                    url_elem.text = url
                all_elems.append(url_elem)
                reason_scraped_elem = ET.SubElement(doc_elem, 'field', {'name': 'reason_scraped'})
                reason_scraped_elem.text = '0'
                all_elems.append(reason_scraped_elem)
                source_type_elem = ET.SubElement(doc_elem, 'field', {'name': 'source_type'})
                source_type_elem.text = source_type
                all_elems.append(source_type_elem)

                if 'author' in metadata:
                    if VERBOSE:
                        print('Author: {}'.format(metadata['author']))
                    author_elem = ET.SubElement(doc_elem, 'field', {'name': 'author'})
                    author_elem.text = metadata['author']
                    all_elems.append(author_elem)
                elif VERBOSE:
                    print('Author is unknown')
                site_name_elem = ET.SubElement(doc_elem, 'field', {'name': 'site_name'})
                site_name_elem.text = re.sub('^www\\.', '', parsed_url.netloc)
                all_elems.append(site_name_elem)

                # Text (extracted earlier).
                text_elem = ET.SubElement(doc_elem, 'field', {'name': 'text'})
                text_elem.text = text
                all_elems.append(text_elem)
                if VERBOSE:
                    print('Text: "{}"'.format(re.sub('\\s', ' ', text[:40])))

                # Date retrieved.
                date_retr_elem = ET.SubElement(doc_elem, 'field', {'name': 'date_retr'})
                date_retr_elem.text = date_retrieved
                all_elems.append(date_retr_elem)

                # Date posted.
                if 'date' in metadata:
                    if VERBOSE:
                        print('Date: {}'.format(metadata['date']))
                    date_post_elem = ET.SubElement(doc_elem, 'field', {'name': 'date_post'})
                    # Convert to UTC and the Solr-accepted format. 
                    formatted_date = metadata['date'].astimezone(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
                    if VERBOSE:
                        print('Document date formatted as {}'.format(formatted_date))
                    date_post_elem.text = formatted_date
                    all_elems.append(date_post_elem)
                elif VERBOSE:
                    print('Date is unknown')
            if skipped_no_permalink > 0 and VERBOSE:
                print('---------')
                print('Skipped {} messages on {} due to lack of permalink'.format(skipped_no_permalink, url))
                print('---------')
tree = ET.ElementTree(xml)
tree.write(output_filename, encoding='utf-8')
