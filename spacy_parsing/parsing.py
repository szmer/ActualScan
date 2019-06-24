from spacy_conll import Spacy2ConllParser

spacyconll = Spacy2ConllParser(model='en_core_web_md')

# TODO read from Redis.
with open('input_utterances.txt') as utt_file:
    for utterance in utt_file.readlines():
        if utterance.strip() == '':
            continue
        for parsed_sent in spacyconll.parse(input_str=utterance):
            print(parsed_sent)
            print()
        print()
