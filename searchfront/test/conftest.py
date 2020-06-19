import pytest
from flask_security.utils import hash_password

from flask_instance import settings
from searchfront.app import create_app
from searchfront.extensions import db as _db, socketio as _socketio, user_datastore
from searchfront.scrapy_process import scrapyp as _scrapyp
from searchfront.reddit_process import redditp as _redditp

from searchfront.blueprints.account import AppUser
from searchfront.blueprints.site.models import Tag, Site
from searchfront.blueprints.live_config import LiveConfigValue

TEST_USER_EMAIL = 'test@example.com'
TEST_USER_PASSWORD = 'password'

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

@pytest.yield_fixture(scope='session')
def socketio(app):
    _socketio.init_app(app)
    return _socketio

@pytest.fixture(scope='session')
def db(app):
    _db.drop_all() # this seems to be needed to update schema in Python-side models
    _db.create_all()

    fun_tag_dict = {'name':'fun', 'level':100000, 'description':'Sites containing fun things.'}
    existing_tags = list(Tag.query.filter_by(**fun_tag_dict))
    if len(existing_tags) == 0:
        tag_fun = Tag(**fun_tag_dict)
        _db.session.add(tag_fun)
        _db.session.commit()
    else:
        tag_fun = existing_tags[0]
    games_tag_dict = {'name':'games', 'level':100000, 'description':'Games of all kinds.'}
    existing_tags = list(Tag.query.filter_by(**games_tag_dict))
    if len(existing_tags) == 0:
        tag_games = Tag(**games_tag_dict)
        _db.session.add(tag_games)
        _db.session.commit()
    else:
        tag_games = existing_tags[0]
    reddit_tag_dict = {'name':'reddit', 'level':100000, 'description':'Various subreddits.'}
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
                level=100000,
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
                level=100000,
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
    # Create the roles if needed.
    user_datastore.find_or_create_role(name='admin', description='Administrator')
    user_datastore.find_or_create_role(name='registered', description='Registered user')
    db.session.commit()

    user = user_datastore.get_user(TEST_USER_EMAIL)
    print(user)
    if not user:
        user = user_datastore.create_user(email=TEST_USER_EMAIL,
                password=hash_password(TEST_USER_PASSWORD))
        db.session.commit()
        user_datastore.add_role_to_user(user, 'registered')
        db.session.expunge(user)
        db.session.commit()
    yield user
    AppUser.query.filter_by(email=TEST_USER_EMAIL).delete(synchronize_session=False)
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

@pytest.fixture(scope='function')
def always_issue_perms(db):
    # Temporarily configure the app to always issue guest permissions.
    free_perms_threshold = LiveConfigValue.query.get('guest_scan_permissions_threshold')
    current_free_threshold = int(free_perms_threshold.value)
    free_perms_threshold.value = 0
    db.session.add(free_perms_threshold)
    db.session.commit()

    yield True

    # Reset the threshold to the previous value.
    free_perms_threshold = LiveConfigValue.query.get('guest_scan_permissions_threshold')
    free_perms_threshold.value = current_free_threshold
    db.session.add(free_perms_threshold)
    db.session.commit()

@pytest.fixture(scope='function')
def never_issue_perms(db):
    # Temporarily configure the app to never issue guest permissions.
    free_perms_threshold = LiveConfigValue.query.get('guest_scan_permissions_threshold')
    current_free_threshold = int(free_perms_threshold.value)
    free_perms_threshold.value = 2 * int(LiveConfigValue.query.get(
        'concurrent_jobs_allowed').value)
    db.session.add(free_perms_threshold)
    db.session.commit()

    yield True

    # Reset the threshold to the previous value.
    free_perms_threshold = LiveConfigValue.query.get('guest_scan_permissions_threshold')
    free_perms_threshold.value = current_free_threshold
    db.session.add(free_perms_threshold)
    db.session.commit()
