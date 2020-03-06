import pytest

from config import settings
from searchfront.app import create_app
from searchfront.extensions import db as _db

from searchfront.blueprints.site.models import Site

@pytest.yield_fixture(scope='session')
def app():
    db_uri = '{0}_test'.format(settings.SQLALCHEMY_DATABASE_URI)
    params = {
        'DEBUG': True,
        'SQLALCHEMY_DATABASE_URI': db_uri
    }
    _app = create_app(settings_override=params)
    context = _app.app_context()
    context.push()
    yield _app
    context.pop()

@pytest.fixture(scope='session')
def db(app):
    _db.drop_all()
    _db.create_all()

    site = Site(homepage_url='https://szymonrutkowski.pl/blog',
            search_url='https://szymonrutkowski.pl/blog/?s=|||fat|||+|||cat|||&submit=Search',
            source_type='blog',
            tags='fun,games')
    _db.session.add(site)
    _db.session.commit()

    return _db
