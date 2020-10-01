from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import os
from threading import Thread

import pytest
from scrapy.crawler import CrawlerProcess

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
            response_docs.append([{ 'url': 'http://localhost/secondpage.html' }])
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
    def run(self, httpd):
        httpd.serve_forever()

@pytest.fixture(scope='session')
def mock_solr():
    server_address = ('', 8983)
    httpd = HTTPServer(server_address, MockSolrHandler)
    server_thread = MockServerThread()
    server_thread.run(httpd)
    yield httpd
    httpd.shutdown()

@pytest.fixture(scope='session')
def mock_stractor():
    server_address = ('', 3757)
    httpd = HTTPServer(server_address, MockStractorHandler)
    server_thread = MockServerThread()
    server_thread.run(httpd)
    yield httpd
    httpd.shutdown()

@pytest.fixture(scope='session')
def mock_web_server():
    server_address = ('', 80)
    httpd = HTTPServer(server_address, MockStractorHandler)
    server_thread = MockServerThread()
    server_thread.run(httpd)
    yield httpd
    httpd.shutdown()

@pytest.fixture(scope='session')
def scrapy_process(db, mock_solr, mock_stractor, mock_web_server):
    # NOTE we have to import it here to act within the db fixture, ugly but necessary
    from genscrap.spiders import GeneralSpider
    os.environ['SOLR_HOST'] = 'localhost'
    os.environ['SOLR_PORT'] = '8393'
    os.environ['SPEECHTRACTOR_HOST'] = 'localhost'
    os.environ['SPEECHTRACTOR_PORT'] = '3757'
    process = CrawlerProcess()
    process.crawl(GeneralSpider) # non-blocking
    yield process
    process.stop()
