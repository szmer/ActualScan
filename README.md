# Table of contents
1. Introduction
2. Technical overview:
  - architecture
  - setup instructions
  - testing
  - debugging
3. Contributing and communications

# Introduction

ActualScan is a search engine addressing the problems of browsing the modern Web, while focusing
on informative and niche websites with discussions and articles. It's good for exploring what
people have to say about any topic.
Read more on the visions for [social indexing](https://tech.actualscan.com/posts/social-indexing/)
and [analytic results](https://tech.actualscan.com/posts/analytic-results/) on the blog.

ActualScan is free software (AGPL v3): you can run your own instance and review and modify the code, as long
as you also make the source available to users.

Currently indexing should work decently on most Wordpress sites, some media and forums sites, and Reddit
(optional: you have to use your own [Reddit API key](https://www.reddit.com/wiki/api)).

The project is still in a very early stage! Stay tuned for a public alpha server.
You can follow the [blog](https://tech.actualscan.com/) with the email newsletter or RSS.
Also see the **contributing** info at the end.

# Technical overview

Under the hood ActualScan is a bunch of services written in Python, Java and some Common Lisp. The whole stack is
provided with two orchestration schemes, Docker Compose and Ansible.

Docker Compose is intended for testing and developing ActualScan code on your local machine. We try to
include as much of production-level network security (TLS etc.) as possible, to test that everything works,
but overall this version is not intended for serious production use.

The Ansible version, on the other hand, should offer a relatively plug-and-play experience where you
provide your servers and get a search engine. Due to some recent serious changes in the stack,
and apparently changes in RedHat's CentOS support guarantees, this is currently *not up to date*.
*It is possible to go fully Ansible in the future, but then we would require using virtual machines for local development*.

## List of services (architecture overview)

A guide to exploring the code if you want. The list goes by the Docker Compose container names.

- **Website**: a Django project, at `python_modules/ascan`. The `scan` package (app) contains the main search interface and classes for
controlling crawling/scanning process. The `manager` package contains user interface for managing sites, tags and other entities.
The `bg` package is for everything else, like user profiles, autocomplete etc. *This partitioning scheme isn't ideal, but a replacement
would have to be clearly better and for the long term*.
- **Maintenance** daemon: a constant worker supervising scans (starting, finishing...), sending WebSocket updates etc. The code is in
`python_modules/ascan/maintenance.py`
- **Speechtractor**: a Lisp HTTP service used by the crawler. Gets raw HTML, extracts parsed text, URLs, author info and such. The code
is in `lisp/speechtractor`.
*The continued use of Common Lisp is not certain. It has been already abandoned for Omnivore; the reasoning with pros and cons may
appear on the blog at some point.*
- **Omnivore2**: a Python service used by the crawler and working on the index in the background. It gets the parsed articles (divided
into a-couple-sentences chunks loosely called "[periods](http://dcc.dickinson.edu/grammar/latin/structure-period)") and applies
various text analytics. The texts are then annotated with this info in Solr and this can be used freely for sorting results.
The code lives in `python_modules/omnivore2`.
- A **[StormCrawler](https://stormcrawler.net/)-based crawler** (the zookeeper, nimbus and other Storm cluster members):
this does the actual crawling and fetching pages with reasonable politeness. The process is tightly controlled and monitorable
from the website through objects stored in PostgreSQL. The relevant Storm topology code lives in `java/generalcrawl`.
*There used to exist a convoluted Scrapy spider for this which became unworkable. Not all functionality has been ported, which is a
big priority now*.
- **Redditscrape**: a [PRAW](https://praw.readthedocs.io/en/latest/)-based script, working continuously in parallel to the
regular Web crawler. It uses Reddit API instead of HTML scraping. The code is in `python_modules/ascan/reddit_scraper.py`.
- The **[Selenium](https://www.selenium.dev/)** instance. This is needed for parsing pages relying on JavaScript,
especially search pages. *Currently not used because the relevant crawler code needs to be ported from the old Scrapy spider.*
- The databases: **Solr** for storing and searching web pages, **Redis** for some ephemeral flags and data used by Python code,
**PostgreSQL** for everything else.

## Setting up the Docker Compose version

Sorry that this is a longish, somewhat manual process. **If you're not sure what you're doing, don't!**
The experience should be gradually improved when the project matures.

1. These instructions are written for Linux, but everything we use is cross-platform. Install Docker and Docker Compose.
You will also need openssl, JDK 11+ and Apache Maven installed.
2. Clone this repository. Make a copy of the `.env.sample` file and name it `.env`.
3. Open that file and follow instructions, setting up passwords, secrets etc. for the services. Never
reuse those in production!
4. Create the self-signed SSL certificates for communications between services.
(These are different from public-facing certs you would use when putting ActualScan on the Web.)
In each case use **the same** passphrase that you set as `KEYSTORE_PASSWORD` variable in your `.env` file.
From the `certs/dev` directory run:
```bash
openssl req -x509 -newkey rsa:4096 -keyout ascan_dev_internal.pem -out ascan_dev_internal.pem \
-config ./openssl.cnf -days 365
# Make a copy of the key without the passphrase:
openssl rsa -in ascan_dev_internal.pem -out ascan_dev_internal_key.key
openssl pkcs12 -export -in ascan_dev_internal.pem -inkey ascan_dev_internal.pem \
-out ascan_dev_ssl_internal.keystore.p12 -name "ascan-solr"
```
5. Compile the Java package with the crawler topology. From `java/generalcrawl` run:
```bash
mvn compile
mvn package
```
6. Now you can build the services with `docker-compose build` in the main repo directory.
7. Before starting the website, perform necessary database migrations like so:
The last command will walk you through creating the admin account.
```bash
docker-compose run website python manage.py makemigrations
docker-compose run website python manage.py migrate
docker-compose run website python manage.py createsuperuser
```
8. Finally, you can issue the `docker-compose up` command. Your local ActualScan instance is now up and running!

Troubleshooting:
- If you use and enforce SELinux, the containers may have problems with reading from disk inside them. The
usual symptom is Python complaining about unimportable modules. You can change the rules appropriately
by running `chcon -Rt svirt_sandbox_file_t .` (probably as sudo) in the project directory.
- If things (especially Storm and the `nimbus` container) behave strangely after restarting, try stopping
everything with `docker-compose down` and restarting again with `docker-compose up`.

## Running tests and some tips for debugging

### On Docker Compose

You can run all existing tests in the containers with `bash run-tests-docker-compose.sh`. The containers need
to be up. Python services use [pytest](https://docs.pytest.org/en/latest/contents.html), Common Lisp tests are
scripted with the [fiasco](https://github.com/joaotavora/fiasco) package.

- If you want to dig into some failing tests with `pdb` in one container,
`docker-compose exec website pytest -x --pdb` can be useful (in this case for the `website` service).
- To plug directly into the PostgreSQL console, use `docker-compose exec postgres psql -U <your-postgres-username>`.
- To get more logs from the `redditscrape` service, change `-L INFO` in its entry in  `docker-compose.yml`
to `-L DEBUG`.
- You sometimes can also see more logs from a service with `docker-compose logs <container_name>`.
- To directly inspect the Storm topology (crawler) logs, modify this snippet. The exact directory changes depending
on the timestamp when the crawler topology started.
```bash
docker-compose exec storm_supervisor ls /logs/workers-artifacts
docker-compose exec storm_supervisor cat /logs/workers-artifacts/crawl-1-<THIS PART CHANGES>/6700/worker.log
```

# Contributing and communications
- For your own sake, do not submit patches/pull requests without asking first.
- Main needs and plans for the project are communicated with GitHub issues.
- Your opinions and suggestions are very kindly welcome! (Agreement not guaranteed though 🙃)
- Currently the main channel **for suggestions** is email (see below) or GitHub issues. We will think of opening a mailing list
or a IRC/Matrix room *(or something different but open and reasonable)* if there's a need for it.
- Get in touch especially if you use, or seriously want to use, the software: **write an email** to contact@actualscan.com.
Do communicate your needs, so they can be taken into account in new features and you can get info on important changes.
(Note that this is *only* good will and best effort, as ActualScan is currently developed completely for free.)
