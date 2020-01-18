import os
from datetime import timezone
from urllib.parse import urlparse
import xml.etree.cElementTree as ET
from sentence_splitter import SentenceSplitter
from lxml import html
from jusText_star.justext.core import justext
# WARNING this uses the original installed justext package directory!
from jusText_star.justext.utils import get_stoplist
from date_extractor import extractArticlePublishedDate # file in the same dir

# CONFIG desired range of rows here.
#ids = [1970]
ids = range(1900, 2186)
verbose_ids = [1910, 1944, 1963]

VERBOSE = False

site_types = {
        'head-fi': 'f',
        'forums': 'f',
        'cnet': 'm',
        'majorhifi': 'm',
        'leadsrating': 'm',
        'reddit': 's',
        'homestudio': 'b',
        'headphonedungeon': 'b',
        'soundgearlab': 'b'
        }

output_filename = 'test_solr.xml'

splitter = SentenceSplitter(language='en')

#
# NOTE WHEN SCRAPING WE WANT TO HAVE UTC AS OUR LOCAL TIMEZONE!
#

def is_newdoc(tag, attrs):
    result = (tag == 'article') # on head-fi, stevehuffman, audioholics
    return result

xml = ET.Element('add')
processed_urls = set()
all_elems = [] # keep them for the ET to write
with open(output_filename, 'w+') as output_file:
    for page_id in ids:
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

        with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/index') as html_file:
            multidocs = (source_type == 'f') # true on forums with many posts on one page
            if VERBOSE:
                if multidocs:
                    print('Entering multidocs mode.')
                else:
                    print('One doc per page mode.')

            dom = html.fromstring(str.encode(html_file.read()))
            # (Xpath expression for findding anywhere in the document)
            doc_title = dom.findtext('.//title')
            if VERBOSE:
                print('HTML page title: {}.'.format(doc_title))
            doc_date = False

            if multidocs:
                paragraphs = justext(dom, get_stoplist('English'))
            else:
                paragraphs = justext(dom, get_stoplist('English'))

            if VERBOSE:
                print('{} paragraphs retrieved ({} good, {} separate documents).'.format(
                    len(paragraphs),
                    len([par for par in paragraphs if par.class_type == 'good']),
                    len([par for par in paragraphs if par.docstart])))

            # Collect the documents list of (title, list of sections as strings)
            if multidocs:
                documents = []
                current_sections = []
                pre_doc = True # before the first document/post
                for par in paragraphs:
                    if par.class_type != 'good':
                        continue
                    if par.docstart: # commit the current post
                        if pre_doc:
                            pre_doc = False
                        else:
                            documents.append((doc_title, current_sections))
                            current_sections = []
                    if not pre_doc:
                        current_sections.append(par.text)
                # Commit the last post if needed.
                if len(current_sections) != 0:
                    documents.append((doc_title, current_sections))
            if not multidocs:
                sections = [par.text for par in paragraphs if par.class_type == 'good']
                documents = [(doc_title, sections)]
                doc_date = extractArticlePublishedDate(dom)
                if VERBOSE:
                    print('Determined document date: {}.'.format(doc_date))

            # Write the documents to XML.
            for title, sections in documents:
                text = ''
                for sec_str in sections:
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

                text_elem = ET.SubElement(doc_elem, 'field', {'name': 'text'})
                text_elem.text = text
                all_elems.append(text_elem)

                title_elem = ET.SubElement(doc_elem, 'field', {'name': 'title'})
                title_elem.text = title
                url_elem = ET.SubElement(doc_elem, 'field', {'name': 'url'})
                url_elem.text = url
                all_elems.append(url_elem)
                reason_scraped_elem = ET.SubElement(doc_elem, 'field', {'name': 'reason_scraped'})
                reason_scraped_elem.text = '0'
                all_elems.append(reason_scraped_elem)
                source_type_elem = ET.SubElement(doc_elem, 'field', {'name': 'source_type'})
                source_type_elem.text = source_type
                all_elems.append(source_type_elem)
                # TODO
###                author_elem = ET.SubElement(doc_elem, 'field', {'name': 'author'})
###                author_elem.text = author
###                all_elems.append(author_elem)

                # Date retrieved.
                date_retr_elem = ET.SubElement(doc_elem, 'field', {'name': 'date_retr'})
                date_retr_elem.text = date_retrieved
                all_elems.append(date_retr_elem)
                # Date posted.
                if doc_date:
                    date_post_elem = ET.SubElement(doc_elem, 'field', {'name': 'date_post'})
                    # Convert to UTC and the Solr-accepted format. 
                    formatted_date = doc_date.astimezone(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
                    if VERBOSE:
                        print('Document date formatted as {}'.format(formatted_date))
                    date_post_elem.text = formatted_date
                    all_elems.append(date_post_elem)
                    doc_date = False
tree = ET.ElementTree(xml)
tree.write(output_filename, encoding='utf-8')
