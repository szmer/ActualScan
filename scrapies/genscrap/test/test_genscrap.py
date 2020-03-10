from scrapies.genscrap.genscrap.lib import stractor_reading

class TestGeneralSpider(object):
    def test_stractor_reading(self):
        stractor_output = stractor_reading('<html><body>Hej!</body></html>', 'blog')
        assert stractor_output == '[{"text":""}]'
