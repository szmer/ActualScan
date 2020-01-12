import os
import json
import csv
from spacy_conll_star.spacy_conll import Spacy2ConllParser
from bs4 import BeautifulSoup

spacyconll = Spacy2ConllParser(model='en_core_web_sm')

# CONFIG desired range of rows here.
#ids = [1970]
ids = range(0, 2186)

rowid = 0

def pg_list(lst):
    return str(lst).replace('[', '{').replace(']', '}')

output_filename = 'test_trees.csv'
processed_urls = set()
with open(output_filename, 'w+') as output_file:
    csv_out = csv.writer(output_file, delimiter=';')
    for page_id in ids:
        # (skip files marked as dead - ie. duplicates)
        if os.path.isfile('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/dead'):
            continue
        if not os.path.isfile('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/raw_utterances.json'):
            continue
        with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/raw_utterances.json') as utt_json_file:
            utterances = json.load(utt_json_file)
            if not utterances:
                continue

            # Extract the basic data.
            with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/index') as html_file:
                soup = BeautifulSoup(html_file, features='lxml')
                title = soup.title.string
            with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/url') as url_file:
                url = url_file.read().strip()
            if url in processed_urls:
                continue
            else:
                processed_urls.add(url)

            doc_root_rowid = rowid
            rowid += 1
            doc_rowids = []
            for utt_n, utterance in enumerate(utterances):
                for paragraph in utterance.split('\n'):
                    paragraph_rowids = []
                    if len(paragraph.strip()) == 0:
                        continue
                    par_sents = spacyconll.parse(input_str=paragraph)
                    for sent_n, (sent_obj, sent_tree) in enumerate(par_sents):
                        sentence_rowids = []
                        for tok_n, token_obj in enumerate(sent_obj):
                            # TODO interps appended to token texts!
                            row = [rowid, 'token', 't{}/{}'.format(page_id, rowid),
                                    token_obj.string.strip(), '', doc_root_rowid, '', '']
                            csv_out.writerow(row)
                            sentence_rowids.append(rowid)
                            rowid += 1
                        row = [rowid, 'sentence', 's{}/{}'.format(page_id, rowid),
                                '', pg_list(sentence_rowids), doc_root_rowid,
                                json.dumps({'meta': {'conll_tree': sent_tree}}),
                                '']
                        csv_out.writerow(row)
                        paragraph_rowids.append(rowid)
                        rowid += 1
                    row = [rowid, 'section', 'p{}/{}'.format(page_id, rowid),
                            '', pg_list(paragraph_rowids), doc_root_rowid, '', '']
                    csv_out.writerow(row)
                    doc_rowids.append(rowid)
                    rowid += 1
            row = [rowid, 'document', 'd{}'.format(page_id),
                    '', pg_list(doc_rowids), doc_root_rowid,
                    json.dumps({'title': title, 'meta': {'url': url}}), '']
            csv_out.writerow(row)
