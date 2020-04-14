import pytest
from flask_security.utils import hash_password

from flask_instance import settings
from searchfront.app import create_app
from searchfront.extensions import db as _db, user_datastore
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
    # NOTE ???? it seems that it's important for these to be set AFTER the app creation, or the
    # WTForms CSRF based tests (as frontpage/test_views) will fail
    _app.config['WTF_CSRF_ENABLED'] = False
    _app.config['CSRF_ENABLED'] = False
    _app.config['DEBUG_TB_ENABLED'] = False
    _app.config['ENV'] = 'production'
    context = _app.app_context()
    context.push()
    yield _app
    context.pop()

@pytest.fixture(scope='session')
def db(app):
    _db.create_all()

    fun_tag_dict = {'name':'fun', 'level':'base', 'description':'Sites containing fun things.'}
    existing_tags = list(Tag.query.filter_by(**fun_tag_dict))
    if len(existing_tags) == 0:
        tag_fun = Tag(**fun_tag_dict)
        _db.session.add(tag_fun)
        _db.session.commit()
    else:
        tag_fun = existing_tags[0]
    games_tag_dict = {'name':'games', 'level':'base', 'description':'Games of all kinds.'}
    existing_tags = list(Tag.query.filter_by(**games_tag_dict))
    if len(existing_tags) == 0:
        tag_games = Tag(**games_tag_dict)
        _db.session.add(tag_games)
        _db.session.commit()
    else:
        tag_games = existing_tags[0]
    reddit_tag_dict = {'name':'reddit', 'level':'base', 'description':'Various subreddits.'}
    existing_tags = list(Tag.query.filter_by(**reddit_tag_dict))
    if len(existing_tags) == 0:
        tag_reddit = Tag(**reddit_tag_dict)
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

    # TODO remove the site afterwards?
    existing_sites = list(Site.query.filter_by(site_name='quotes.toscrape.com'))
    if len(existing_sites) == 0:
        site = Site(homepage_url='http://quotes.toscrape.com',
                level='base',
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
    existing_sites = list(Site.query.filter_by(site_name='/r/test'))
    if len(existing_sites) == 0:
        site = Site(homepage_url='https://reddit.com/r/test',
                level='base',
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
def example_user(db):
    user = user_datastore.get_user('test@example.com')
    if not user:
        user = user_datastore.create_user(email='test@example.com',
                password=hash_password('password'))
    db.session.commit()
    user_datastore.add_role_to_user('test@example.com', 'registered')
    db.session.commit()
    yield user
    db.session.delete(user)
    db.session.commit()

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
