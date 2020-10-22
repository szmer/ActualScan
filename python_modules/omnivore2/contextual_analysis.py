from nlp_setup import nlp
from period import period_text
from stationary_analysis import STATIONARY_ANALYTIC_FUNS
from utils import date_fmt, time_now

def add_average_tfidf(doc_dicts, spacy_period_docs):
    """
    Add a average_word_tf_idf_f entry to the period-alist, containing the average tf-idf score
    for a word taken from the period.
    """
    term_doc_freqs = dict()
    for period_doc in spacy_period_docs:
        for token in period_doc:
            token_str = str(token)
            if token_str in term_doc_freqs:
                term_doc_freqs[token_str] += 1
            else:
                term_doc_freqs[token_str] = 1
    for doc_n, doc_dict in enumerate(doc_dicts):
        accumulated_tf_idf = 0
        for token in spacy_period_docs[doc_n]:
            accumulated_tf_idf += 1 / term_doc_freqs[str(token)] # the know df (1) times the idf
        doc_dict['average_word_tf_idf_f'] = accumulated_tf_idf / len(spacy_period_docs[doc_n])
    return doc_dicts

def add_sentences_length_deviation(doc_dicts, spacy_period_docs):
    """
    Add a sentence_length_deviation_f entry to the period-alist, containing the average deviation
    of the length of a sentence (in chars) from a period, when compared to the context. The number can
    be positive or negative depending on the direction of the deviation.
    """
    accumulated_sentence_length = 0.0
    sentence_count = 0
    for period_doc in spacy_period_docs:
        for sent in period_doc.sents:
            accumulated_sentence_length += len(str(sent))
        sentence_count += len(list(period_doc.sents))
    average_length = accumulated_sentence_length / sentence_count
    for doc_n, doc_dict in enumerate(doc_dicts):
        sentences_length = sum([len(str(sent)) for sent in spacy_period_docs[doc_n].sents])
        sentences_average_length = sentences_length / len(list(spacy_period_docs[doc_n].sents))
        doc_dict['sentence_length_deviation_f'] = sentences_average_length - average_length
    return doc_dicts

CONTEXTUAL_ANALYTIC_FUNS = [add_average_tfidf, add_sentences_length_deviation]

def apply_contextual_analysis(doc_dicts):
    spacy_period_docs = [nlp(period_text(doc)) for doc in doc_dicts]
    for analysis_fun in CONTEXTUAL_ANALYTIC_FUNS:
        analysis_fun(doc_dicts, spacy_period_docs)
    # Add/update the date_class and possibly add the missing stationary analytics.
    for doc_n, doc_dict in enumerate(doc_dicts):
        doc_dict['date_class'] = date_fmt(time_now())
        for field_name, analysis_fun in STATIONARY_ANALYTIC_FUNS.items():
            if not field_name in doc_dict:
                analysis_fun(doc_dict, spacy_period_docs[doc_n])
    return doc_dicts
