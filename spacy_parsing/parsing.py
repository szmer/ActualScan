from spacy_conll import Spacy2ConllParser
import json

spacyconll = Spacy2ConllParser(model='en_core_web_lg')

#with open('input_utterances.txt') as utt_file:
#    for utterance in utt_file.readlines():
with open('/home/szymon/lingwy/therminsley/sitesdb/1970/raw_utterances.json') as utt_json_file:
    utterances = json.load(utt_json_file)
    for utterance in utterances:
        print('#####')
        # this changes nothing in practice
        ##if utterance.strip() == '' or not ('solation' in utterance):
        ##    continue
        for parsed_sent in spacyconll.parse(input_str=utterance):
            print(parsed_sent)
            print()
        print()
