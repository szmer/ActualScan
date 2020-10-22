from contextual_analysis import (
        add_sentences_length_deviation, add_average_tfidf, apply_contextual_analysis
        )
from nlp_setup import nlp
from period import period_text

class TestStationaryAnalysis():
    def test_sentences_length_deviation(self):
        test_docs = [
                { 'text_en': 'Slowly by degrees as one million of years followed another, this fiery scene would lose its eruptive incandescence.', 'language_code': 'en' },
                { 'text_en': 'The vapours in the sky would rain down and become less dense overhead; great slaggy cakes of solidifying rock would appear upon the surface of the molten sea, and sink under it to be replaced by other floating masses.', 'language_code': 'en' },
                { 'text_en': 'The sun and moon growing now each more distant and each smaller, would rush with diminishing swiftness across the heavens.', 'language_code': 'en' },
                ]
        spacy_period_docs = [nlp(period_text(doc)) for doc in test_docs]
        add_sentences_length_deviation(test_docs, spacy_period_docs)
        assert 'sentence_length_deviation_f' in test_docs[0]
        assert 'sentence_length_deviation_f' in test_docs[2]
        assert test_docs[1]['sentence_length_deviation_f'] > 0.0
        assert test_docs[0]['sentence_length_deviation_f'] < 0.0

    def test_average_tfidf(self):
        test_docs = [
                { 'text_en': 'Slowly by degrees as one million of years followed another, this fiery scene would lose its eruptive incandescence.', 'language_code': 'en' },
                { 'text_en': 'The vapours in the sky would rain down and become less dense overhead; great slaggy cakes of solidifying rock would appear upon the surface of the molten sea, and sink under it to be replaced by other floating masses.', 'language_code': 'en' },
                { 'text_en': 'whargarbl ninja', 'language_code': 'en' },
                ]
        spacy_period_docs = [nlp(period_text(doc)) for doc in test_docs]
        add_average_tfidf(test_docs, spacy_period_docs)
        assert 'average_word_tf_idf_f' in test_docs[0]
        assert test_docs[2]['average_word_tf_idf_f'] > test_docs[1]['average_word_tf_idf_f']

    def test_apply_contextual_analysis(self):
        test_docs = [
                { 'text_en': 'Slowly by degrees as one million of years followed another, this fiery scene would lose its eruptive incandescence.', 'language_code': 'en' },
                { 'text_en': 'The vapours in the sky would rain down and become less dense overhead; great slaggy cakes of solidifying rock would appear upon the surface of the molten sea, and sink under it to be replaced by other floating masses.', 'language_code': 'en' },
                { 'text_en': 'The sun and moon growing now each more distant and each smaller, would rush with diminishing swiftness across the heavens.', 'language_code': 'en' },
                ]
        apply_contextual_analysis(test_docs)
        assert 'sentence_length_deviation_f' in test_docs[0]
        assert 'date_class' in test_docs[0]
        assert 'average_word_tf_idf_f' in test_docs[1]
        assert 'date_class' in test_docs[1]
        # performing the missing stationary analysis:
        assert 'average_word_length_f' in test_docs[2]
