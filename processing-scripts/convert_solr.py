import json
import os
import re
from urllib.parse import urlparse
import requests

#
# CONFIG desired range of rows here.
#
#ids = [1970]
ids = range(0, 2147)#2326)
verbose_ids = [2249]

SPEECHTRACTOR_ADDR = 'http://localhost:3756/api/v01/interpret'

VERBOSE = False

output_filename = 'headphones.json'

site_types = {
        'dazeddigital.com': 'media',
        'fashionista.com': 'media',
        'glamour.com': 'media',
        'thefashionpolice.net': 'media',
        'vogue.com': 'media',
        'wwd.com': 'media',
        'askandyaboutclothes.com': 'forums',
        'edcforums.com': 'forums',
        'forums.redflagdeals.com': 'forums',
        'head-fi.org': 'forums',
        'styleforum.net': 'forums',
        'forums.thefashionspot.com': 'forums',
        'thestudentroom.co.uk': 'forums',
        'wallstreetoasis.com': 'forums',
        'youlookfab.com': 'forums', # NOTE we look at domain, but really its the /welookfab directory!
        'dieworkwear.com': 'blog',
        'effortlesseverydaystyle.blogspot.com': 'blog',
        'kendieveryday.com': 'blog',
        'pennypincherfashion.com': 'blog',
        'themodestman.com': 'blog',
###        'fashionbyina': 'blog',
###        'frugalfashionshopper': 'blog',
###        'homestudio': 'blog',
###        'headphonedungeon': 'blog',
###        'jseverydayfashion': 'blog',
###        'majorhifi': 'media',
###        'leadsrating': 'media',
###        'thefashionables': 'media',
###        'cnet': 'media',
        }

site_tags = {
        'dazeddigital.com': ['fashion'],
        'fashionista.com': ['fashion'],
        'glamour.com': ['fashion'],
        'thefashionpolice.net': ['fashion'],
        'vogue.com': ['fashion'],
        'wwd.com': ['fashion'],
        'askandyaboutclothes.com': ['fashion'],
        'edcforums.com': ['fashion'],
        'forums.redflagdeals.com': ['fashion'],
        'head-fi.org': ['hifi'],
        'styleforum.net': ['fashion'],
        'forums.thefashionspot.com': ['fashion'],
        'thestudentroom.co.uk': ['fashion'],
        'wallstreetoasis.com': ['fashion'],
        'youlookfab.com': ['fashion'], # NOTE we look at domain, but really its the /welookfab directory!
        'dieworkwear.com': ['fashion'],
        'effortlesseverydaystyle.blogspot.com': ['fashion'],
        'kendieveryday.com': ['fashion'],
        'pennypincherfashion.com': ['fashion'],
        'themodestman.com': ['fashion'],
        }

#
# Getting and filing documents.
#
processed_urls = set()
all_docs = []
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
                tags = site_tags[string]
                break
        else:
            if VERBOSE:
                print('Cannot determine source type, skipping.')
            continue
        if not tags:
            if VERBOSE:
                print('Cannot determine site tags, skipping.')
            continue


        #
        # Process the HTML index.
        #
        with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/index') as html_file:

            payload = { 'html': html_file.read(), 'sourcetype': source_type }
            interpreted_docs = requests.post(SPEECHTRACTOR_ADDR, data=payload, timeout=300)
            ##-print(interpreted_docs.text)
            interpreted_docs = interpreted_docs.json()

            if len(interpreted_docs) == 0:
                print('No documents found inside, skipping.')
                continue

            # For these speechtractor shouldn't find the permalinks.
            if source_type == 'media' or source_type == 'blog':
                assert len(interpreted_docs) == 1
                interpreted_docs[0]['url'] = url
            # Fill missing url fragments in permalinks.
            elif source_type == 'forums':
                for doc in interpreted_docs:
                    if not 'url' in doc: # without a permalink, we have to ignore it
                        continue
                    parsed_permalink = urlparse(doc['url'])
                    if not parsed_permalink.netloc:
                        parsed_permalink = parsed_permalink._replace(netloc=parsed_url.netloc)
                        parsed_permalink = parsed_permalink._replace(scheme=parsed_url.scheme)
                    if not parsed_permalink.path:
                        parsed_permalink = parsed_permalink._replace(path=parsed_url.path)
                    doc['url'] = parsed_permalink.geturl()
            else:
                raise ValueError

            # Write the documents to XML.
            skipped_no_permalink = 0
            skipped_no_author = 0
            skipped_no_text = 0
            for doc in interpreted_docs:
                if not 'url' in doc or not doc['url']:
                    doc['dead'] = True
                    skipped_no_permalink += 1
                    continue
                if not 'text' in doc or not doc['text']:
                    doc['dead'] = True
                    skipped_no_text += 1
                    continue
                if not 'author' in doc or not doc['author']:
                    doc['dead'] = True
                    skipped_no_author += 1
                    continue

                doc['tags'] = tags
                doc['date_retr'] = date_retrieved
                doc['reason_scraped'] = '0'
                doc['source_type'] = source_type[0] # we use the first char
                doc['site_name'] = re.sub('^www\\.', '', parsed_url.netloc)
                if not source_type == 'forums':
                    doc['real_doc'] = 'self'
                else:
                    doc['real_doc'] = url
            all_docs += [doc for doc in interpreted_docs if not 'dead' in doc]

            if VERBOSE and [skipped_no_permalink, skipped_no_text, skipped_no_author] != [0, 0, 0]:
                print('---------')
                print('Skipped {} messages on {} due to lack of permalink'.format(skipped_no_permalink, url))
                print('Skipped {} messages on {} due to lack of text'.format(skipped_no_text, url))
                print('Skipped {} messages on {} due to lack of author'.format(skipped_no_author, url))
                print('---------')
with open(output_filename, 'w+') as output_file:
    json.dump(all_docs, output_file, indent=2, separators=(',', ': '))
