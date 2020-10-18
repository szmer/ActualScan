from solr_get_context import solr_query_field_for_period, solr_boost_field_for_period

class TestSolrContext():
    def test_solr_query_field_for_period(self):
        test_period_dict = {
                'average_word_length_f': 3.98, 'period_number_i': 1,
                'parent_document_length_i': 3, 'sent_length_i': 2, 'word_length_i': 38,
                'text': "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size.",
                'url': "http://example.com/hgwells/shorthistory/carbon200.html",
                'tags': "carbon paleozoic prehistory",
                'date_post': "1922-12-02T13:14:00Z",
                'date_retr': "1966-12-02T13:14:00Z"}
        assert solr_query_field_for_period(test_period_dict) == "q=tags:carbon paleozoic prehistory"

    def test_solr_boost_field_for_period(self):
        test_period_dict = {
                'average_word_length_f': 3.98, 'period_number_i': 1,
                'parent_document_length_i': 3, 'sent_length_i': 2, 'word_length_i': 38,
                'text': "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size.",
                'url': "http://example.com/hgwells/shorthistory/carbon200.html",
                'tags': "carbon paleozoic prehistory",
                'date_post': "1922-12-02T13:14:00Z",
                'date_retr': "1966-12-02T13:14:00Z"}
        test_boost_field = solr_boost_field_for_period(test_period_dict)
        assert test_boost_field.startswith('bf=')
        assert 'average_word_length_f' in test_boost_field
        assert 'date_post' in test_boost_field
        assert 'recip(' in test_boost_field
        assert not 'date_retr' in test_boost_field
        assert not 'date_class' in test_boost_field
