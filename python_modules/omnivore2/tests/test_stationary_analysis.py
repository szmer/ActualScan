from nlp_setup import nlp
from stationary_analysis import (
        add_average_word_length, add_pos_proportions, add_sentiment_info,
        periods_from_solr_document, stationary_analysis_applied
        )

class TestStationaryAnalysis():
    def test_periods_from_document(self):
        test_doc = {
                'text': "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate.",
                'url': "http://example.com/hgwells/shorthistory/carbon200.html",
                'date_post': "1922-12-02T13:14:00Z"
                }
        # note that the inteded period sizes depend on the configuration in nlp_setup
        test_periods = periods_from_solr_document(test_doc)
        assert len(test_periods) == 3
        assert not 'text' in test_periods[0]
        assert 'text_en' in test_periods[0]
        assert test_periods[0]['text_en'] != test_doc['text']
        assert 'en' == test_periods[0]['language_code']
        # The identifier must be added.
        assert test_periods[0]['doc_location'] == 'http://example.com/hgwells/shorthistory/carbon200.html\t1'
        # Preserve the original features.
        assert 'url' in test_periods[0]
        assert 'date_post' in test_periods[0]
        # See if the additional features are present.
        assert 'word_length_i' in test_periods[0]
        assert 'sent_length_i' in test_periods[0]
        assert 'parent_document_length_i' in test_periods[0]
        assert 'period_number_i' in test_periods[0]
        # We want one-based period numbers.
        assert int(test_periods[0]['period_number_i']) == 1

    def test_average_word_length(self):
        test_doc = {
                'text': "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate.",
                'url': "http://example.com/hgwells/shorthistory/carbon200.html",
                'date_post': "1922-12-02T13:14:00Z"
                }
        spacy_doc = nlp(test_doc['text'])
        add_average_word_length(test_doc, spacy_doc)
        assert 'average_word_length_f' in test_doc
        assert isinstance(test_doc['average_word_length_f'], float)

    def test_pos_proportions(self):
        test_doc = {
                'text_en': "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate.",
                'language_code': 'en',
                'url': "http://example.com/hgwells/shorthistory/carbon200.html",
                'date_post': "1922-12-02T13:14:00Z"
                }
        spacy_doc = nlp(test_doc['text_en'])
        add_pos_proportions(test_doc, spacy_doc)
        assert 'adjectives_proportion_f' in test_doc
        assert 'nouns_proportion_f' in test_doc
        assert 'proper_names_proportion_f' in test_doc
        assert 'verbs_proportion_f' in test_doc
        assert isinstance(test_doc['verbs_proportion_f'], float)
        assert test_doc['proper_names_proportion_f'] < test_doc['verbs_proportion_f']

    def test_sentiment_info(self):
        test_doc = {
                'text_en': "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate.",
                'language_code': 'en',
                'url': "http://example.com/hgwells/shorthistory/carbon200.html",
                'date_post': "1922-12-02T13:14:00Z"
                }
        spacy_doc = nlp(test_doc['text_en'])
        add_sentiment_info(test_doc, spacy_doc)
        assert 'positive_polarity_f' in test_doc
        assert 'negative_polarity_f' in test_doc
        assert 'neutral_polarity_f' in test_doc
        assert 'compound_polarity_f' in test_doc
        assert isinstance(test_doc['neutral_polarity_f'], float)
        assert isinstance(test_doc['compound_polarity_f'], float)

    def test_stationary_analysis_applied(self):
        test_doc = {
                'text': "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate.",
                'url': "http://example.com/hgwells/shorthistory/carbon200.html",
                'date_post': "1922-12-02T13:14:00Z"
                }
        test_periods = stationary_analysis_applied([test_doc])
        assert 'text_en' in test_periods[0]
        assert 'date_post' in test_periods[0]
        assert 'word_length_i' in test_periods[0]
        assert 'average_word_length_f' in test_periods[0]
