import spacy

from period import (
        common_strings_count, period_text, periods_from_spacy_sentences, text_char_ngrams
        )

class TestPeriods():
    def test_text_char_ngrams(self):
        assert(text_char_ngrams(3, "The bear could bear it no more.")) == [
                " be" ," be", " co", " it" ," mo", " no", "The", "ar ", "ar ", "bea", "bea", "cou",
                "d b", "e b", "ear", "ear", "he ", "it ", "ld ", "mor", "no ", "o m", "ore", "oul",
                "r c", "r i", "t n", "uld"]

    def test_common_string_count(self):
        assert common_strings_count(text_char_ngrams(3, "The bear could bear it no more."),
                text_char_ngrams(3, "No jumping near me, the badger said.")) == 4
        # The result is the same if we change the argument order.
        assert (common_strings_count(text_char_ngrams(3, "The bear could bear it no more."),
                text_char_ngrams(3, "No jumping near me, the badger said."))
                == common_strings_count(text_char_ngrams(3, "No jumping near me, the badger said."),
                    text_char_ngrams(3, "The bear could bear it no more.")))

    def test_periods_from_sentences(self):
        text = "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate."
        # note that the inteded period sizes depend on the configuration in nlp_setup
        nlp = spacy.load("en_core_web_sm")
        test_periods = periods_from_spacy_sentences(nlp(text).sents)[0] # get only the first value
        assert len(test_periods) == 3
        assert len(test_periods[0]) > 150
        assert len(test_periods[0]) < 250
        assert len(test_periods[1]) > 150
        assert len(test_periods[1]) < 250
        assert len(test_periods[2]) > 150
        assert len(test_periods[2]) < 250

    def test_period_text(self):
        assert period_text({'text_en': 'Hej sokoły', 'language_code': 'en' }) == 'Hej sokoły'
        assert period_text({'text_xx': 'благодарю', 'language_code': 'ru' }) == 'благодарю'
