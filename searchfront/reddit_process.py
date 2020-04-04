import subprocess

class RedditProcess(object):
    process = False

    def run(self):
        self.process = subprocess.Popen(['python3', 'reddit_scraper.py', '-L', 'INFO'],
                cwd='/searchfront/scrapies/')

redditp = RedditProcess()
