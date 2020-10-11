from http.server import BaseHTTPRequestHandler, HTTPServer
import json
from threading import Thread

import pytest
from scrapy.crawler import CrawlerProcess
from scrapy.utils.log import configure_logging
from scrapy.utils.project import get_project_settings

class MockSolrHandler(BaseHTTPRequestHandler):
    def _set_headers(self):
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()

    def do_GET(self):
        self._set_headers()
        if 'ping' in self.path:
            self.wfile.write(json.dump({
                "responseHeader": {
                    "zkConnected": None,
                    "status": 0,
                    "QTime": 50,
                    "params": { "q": "{!lucene}*:*", "distrib": "false", "df": "_text_",
                        "rows": "10", "echoParams": "all"}},
                    "status": "OK"}))
        # This should only appease the solr_check_urls mechanism for deduplication.
        elif 'select' in self.path:
            self.wfile.write(json.dump({ 'response': {'docs': [] }}))
        # This empty response should be fine for receiving updates.
        else:
            self.wfile.write(json.dump({ }))

class MockStractorHandler(BaseHTTPRequestHandler):
    def _set_headers(self):
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()

    def do_POST(self):
        self._set_headers()
        content_len = int(self.headers.getheader('content-length', 0))
        post_data = self.rfile.read(content_len)
        post_json = json.loads(post_data)
        response_docs = []
        if 'sourcetype' in post_json and post_json['sourcetype'] == 'searchpage':
            response_docs.append([{ 'url': 'http://127.0.0.1/secondpage.html' }])
        else:
            response_docs.append([{ 'author': 'Imhotep', 'text': 'ancient wisdom',
                'date_post': '1990-01-01T12:00:00Z' }])
        self.wfile.write(json.dump(response_docs))

class MockWebHandler(BaseHTTPRequestHandler):
    def _set_headers(self):
        self.send_response(200)
        self.send_header('Content-type', 'text/html')
        self.end_headers()

    def do_GET(self):
        self._set_headers()
        self.wfile.write('<html><body>hello</body></html>')

class MockServerThread(Thread):
    def __init__(self, httpd, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.httpd = httpd

    def run(self):
        self.httpd.serve_forever()

@pytest.fixture
def mock_solr():
    server_address = ('', 8983)
    httpd = HTTPServer(server_address, MockSolrHandler)
    server_thread = MockServerThread(httpd)
    server_thread.start()
    yield httpd
    httpd.shutdown()

@pytest.fixture
def mock_stractor():
    server_address = ('', 3757)
    httpd = HTTPServer(server_address, MockStractorHandler)
    server_thread = MockServerThread(httpd)
    server_thread.start()
    yield httpd
    httpd.shutdown()

@pytest.fixture
def mock_web_server():
    # NOTE the same for scrapy gives "connection refused", it doesn't see those servers
###-    params = urllib.parse.urlencode({'html': 'halo', 'sourcetype': 'blogs',
###-        'emptyurl': '1' })
###-    headers = {"Content-type": "application/x-www-form-urlencoded",
###-            "Accept": "text/json"}
###-    conn = http.client.HTTPConnection('127.0.0.1', port=3757, timeout=15)
###-    conn.request('POST', '/api/v01/interpret', params, headers)
###-    print(conn.getresponse())
    server_address = ('', 80)
    httpd = HTTPServer(server_address, MockStractorHandler)
    server_thread = MockServerThread(httpd)
    server_thread.start()
    yield httpd
    httpd.shutdown()

@pytest.fixture
def scrapy_process(db, mock_solr, mock_stractor, mock_web_server):
    # NOTE we have to import it here to act within the db fixture, ugly but necessary
    from genscrap.spiders import GeneralSpider
    #os.environ['SOLR_HOST'] = '127.0.0.1'
    #os.environ['SOLR_PORT'] = '8393'
    #os.environ['SPEECHTRACTOR_HOST'] = '127.0.0.1'
    #os.environ['SPEECHTRACTOR_PORT'] = '3757'
    settings = get_project_settings()
    settings.update({ 'LOG_LEVEL': 'DEBUG', 'EXTENSIONS': {} })
    configure_logging(settings)
    process = CrawlerProcess(settings)
    process.crawl(GeneralSpider) # non-blocking
    yield process
    process.stop()
