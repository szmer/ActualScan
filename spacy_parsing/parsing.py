from spacy_conll import Spacy2ConllParser
import json

spacyconll = Spacy2ConllParser(model='en_core_web_lg')

#ids = [1970]
ids = range(1938)

#with open('input_utterances.txt') as utt_file:
#    for utterance in utt_file.readlines():
for page_id in ids:
    with open('/home/szymon/lingwy/therminsley/sitesdb/'+str(page_id)+'/raw_utterances.json') as utt_json_file:
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
