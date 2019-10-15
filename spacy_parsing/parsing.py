from spacy_conll import Spacy2ConllParser
import os
import json

spacyconll = Spacy2ConllParser(model='en_core_web_sm')

# CONFIG desired range of rows here.
#ids = [1970]
ids = range(2148, 2186)

#with open('input_utterances.txt') as utt_file:
#    for utterance in utt_file.readlines():
for page_id in ids:
    # (skip files marked as dead - ie. duplicates)
    if os.path.isfile('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/dead'):
        continue
    with open('/home/szymonrutkowski/therminsley/sitesdb/'+str(page_id)+'/raw_utterances.json') as utt_json_file:
        utterances = json.load(utt_json_file)
        for utterance in utterances:
            for paragraph in utterance.split('\n'):
                if len(paragraph.strip()) == 0:
                    continue
                print('#####')
                # this changes nothing in practice
                ##if utterance.strip() == '' or not ('solation' in utterance):
                ##    continue
                for parsed_sent in spacyconll.parse(input_str=paragraph):
                    print(parsed_sent)
