from copy import copy

from nlp_setup import nlp
from period import lang_text_field_code, period_text, periods_from_spacy_sentences

def add_period_average_word_length(period, period_doc):
    # this should be a float
    period['average_word_length_f'] = sum([len(token) for token in period_doc]) / len(period_doc)

def periods_from_solr_document(doc_dict):
    spacy_doc = nlp(doc_dict['text'])
    sentence_groups = [] # each group contains a sequence of sents in the same language
    group_langs = []
    for sentence in spacy_doc.sents:
        if not sentence_groups:
            sentence_groups.append([sentence])
            group_langs.append(sentence._.language['language'])
        elif group_langs[-1] == sentence._.language['language']:
            sentence_groups[-1].append(sentence)
        else:
            sentence_groups.append([sentence])
            group_langs.append(sentence._.language['language'])
    all_period_dicts = []
    for group_n, group in enumerate(sentence_groups):
        group_periods, period_sent_counts, period_word_counts = periods_from_spacy_sentences(group)
        for period_n, period_str in enumerate(group_periods):
            period_dict = copy(doc_dict)
            period_dict[lang_text_field_code(group_langs[group_n])] = period_str
            del period_dict['text']
            period_dict['language_code'] = group_langs[group_n]
            period_dict['period_number_i'] = period_n + 1
            period_dict['sent_length_i'] = period_sent_counts[period_n]
            period_dict['word_length_i'] = period_word_counts[period_n]
            period_dict['doc_location'] = doc_dict['url'] + '\t' + str(period_n+1)
            all_period_dicts.append(period_dict)
    for period_dict in all_period_dicts:
        period_dict['parent_document_length_i'] = len(all_period_dicts)
    return all_period_dicts 

STATIONARY_ANALYTIC_FUNS = [add_period_average_word_length]

def stationary_analysis_applied(doc_dicts):
    doc_period_dicts = [periods_from_solr_document(doc_dict) for doc_dict in doc_dicts]
    result_dicts = []
    for period_dicts in doc_period_dicts: # process the periods from each document
        for period in period_dicts:
            period_doc = nlp(period_text(period))
            for analysis_fun in STATIONARY_ANALYTIC_FUNS:
                analysis_fun(period, period_doc)
            result_dicts.append(period)
    return result_dicts 
