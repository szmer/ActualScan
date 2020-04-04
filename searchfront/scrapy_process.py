import subprocess

class ScrapyProcess(object):
    process = False

    def run(self):
        self.process = subprocess.Popen(['scrapy', 'crawl', 'general', '-L', 'INFO'],
                cwd='/searchfront/scrapies/genscrap')

scrapyp = ScrapyProcess()
