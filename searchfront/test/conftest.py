import pytest

from flask_instance import settings
from searchfront.app import create_app
from searchfront.extensions import db as _db
from searchfront.scrapy_process import scrapyp as _scrapyp
from searchfront.reddit_process import redditp as _redditp

from searchfront.blueprints.site.models import Tag, Site
from searchfront.blueprints.live_config import LiveConfigValue

@pytest.yield_fixture(scope='session')
def app():
    # NOTE we use the production database because otherwise we would probably need a separate Scrapy
    # thread??
    db_uri = settings.SQLALCHEMY_DATABASE_URI
    params = {
        'DEBUG': True,
        'SQLALCHEMY_DATABASE_URI': db_uri
    }
    _app = create_app(settings_override=params)
    _app.config.from_object('flask_config.settings')
    context = _app.app_context()
    context.push()
    yield _app
    context.pop()

@pytest.fixture(scope='session')
def db(app):
    _db.drop_all()
    _db.create_all()

    existing_tags = list(Tag.query.filter_by(name='fun'))
    if len(existing_tags) == 0:
        tag_fun = Tag(name='fun')
        _db.session.add(tag_fun)
        _db.session.commit()
    else:
        tag_fun = existing_tags[0]
    existing_tags = list(Tag.query.filter_by(name='games'))
    if len(existing_tags) == 0:
        tag_games = Tag(name='games')
        _db.session.add(tag_games)
        _db.session.commit()
    else:
        tag_games = existing_tags[0]
    existing_tags = list(Tag.query.filter_by(name='reddit'))
    if len(existing_tags) == 0:
        tag_reddit = Tag(name='reddit')
        _db.session.add(tag_reddit)
        _db.session.commit()
    else:
        tag_reddit = existing_tags[0]

    # Pre-populate live config.
    existing_config_rows = list(_db.session.query(LiveConfigValue).all())
    existing_config_keys = [val.key for val in existing_config_rows]
    for key, value in app.config['LIVECONFIG_START_VALUES'].items():
        if not key in existing_config_keys:
            new_config_row = LiveConfigValue(key=key, value=value)
            _db.session.add(new_config_row)
            _db.session.commit()

    # TODO remove the site afterwards
    existing_sites = list(Site.query.filter_by(homepage_url='https://szymonrutkowski.pl/blog'))
    if len(existing_sites) == 0:
        site = Site(homepage_url='http://quotes.toscrape.com',
                search_pointer='http://quotes.toscrape.com/tag/|||fat|||+|||cat|||/page/1',
                source_type='blog', site_name='quotes.toscrape.com',
                site_type='web',
                tags=[tag_fun, tag_games])
        _db.session.add(site)
        _db.session.commit()
    else:
        assert tag_fun in existing_sites[0].tags
        assert tag_games in existing_sites[0].tags 
    # We neet a small subreddit to use minimal amount of data.
    existing_sites = list(Site.query.filter_by(homepage_url='https://reddit.com/r/test'))
    if len(existing_sites) == 0:
        site = Site(homepage_url='https://reddit.com/r/test',
                search_pointer='test',
                source_type='forums', site_name='/r/test',
                site_type='reddit',
                tags=[tag_fun, tag_reddit])
        _db.session.add(site)
        _db.session.commit()
    else:
        assert tag_fun in existing_sites[0].tags
        assert tag_reddit in existing_sites[0].tags

    return _db

@pytest.yield_fixture(scope='session')
def scrapyp():
    if not _scrapyp.process:
        _scrapyp.run()
    
    return _scrapyp

@pytest.yield_fixture(scope='session')
def redditp():
    if not _redditp.process:
        _redditp.run()

    return _redditp
