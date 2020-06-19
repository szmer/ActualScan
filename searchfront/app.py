import logging

from flask import Flask
from flask_security.utils import hash_password

from flask_bootstrap import Bootstrap
from searchfront.extensions import (
        socketio, debug_toolbar, db, csrf, security, user_datastore, admin,
        SiteModelView, TagModelView, SiteModelViewForRegistered, TagModelViewForRegistered,
        ChangeRequestModelView
        )
from searchfront.scrapy_process import scrapyp
from searchfront.reddit_process import redditp

# Import blueprints now as some of them may require setting up extensions beforehand.
from searchfront.blueprints.frontpage import frontpage
from searchfront.blueprints.account import account, AppUser
from searchfront.blueprints.manager import manager, ManagerAdminView, ChangeRequest
from searchfront.blueprints.live_config import LiveConfigValue
from searchfront.blueprints.site import Site, Tag
from searchfront.blueprints.scan_schedule import (scan_schedule, handle_scan_progress,
        ScanJob, ScrapeRequest)

def create_app(settings_override=None):
    app = Flask(__name__, instance_path='/flask_instance',
            instance_relative_config=True)

    app.config.from_object('flask_config.settings')
    # Overwrite with the settings from the instance_path set above
    app.config.from_pyfile('settings.py', silent=True)

    if settings_override:
        app.config.update(settings_override)

    app.logger.addHandler(logging.StreamHandler())
    app.logger.setLevel(app.config['LOG_LEVEL'])

    app.register_blueprint(frontpage)
    app.register_blueprint(scan_schedule)
    app.register_blueprint(account)
    app.register_blueprint(manager)
    extensions(app)
    install_admin(app, db)

    @app.before_first_request
    def init_db():
        with app.app_context():
            db.create_all()

            existing_config_rows = list(db.session.query(LiveConfigValue).all())
            existing_config_keys = [val.key for val in existing_config_rows]
            for key, value in app.config['LIVECONFIG_START_VALUES'].items():
                if not key in existing_config_keys:
                    new_config_row = LiveConfigValue(key=key, value=value)
                    db.session.add(new_config_row)
                    db.session.commit()

            # Create the roles (see also conftest).
            user_datastore.find_or_create_role(name='admin', description='Administrator')
            user_datastore.find_or_create_role(name='registered', description='Registered user')
            db.session.commit()

            # Add the root user.
            if not user_datastore.get_user(app.config['INIT_USER_EMAIL']):
                user_datastore.create_user(email=app.config['INIT_USER_EMAIL'],
                        handle='admin',
                        password=hash_password(app.config['INIT_USER_PASSWORD']))
            db.session.commit()
            user_datastore.add_role_to_user(app.config['INIT_USER_EMAIL'], 'admin')
            db.session.commit()

            # Add the test user when in debug mode.app.config['INIT_USER_EMAIL']
            if app.config['ENV'] == 'development':
                if not user_datastore.get_user('john@example.com'):
                    user_datastore.create_user(email='john@example.com',
                            password=hash_password('password'),
                            handle='john')
                db.session.commit()
                user_datastore.add_role_to_user('john@example.com', 'registered')
                db.session.commit()

    @app.before_first_request
    def init_scrapy():
        scrapyp.run()
        if scrapyp.process.poll() is None:
            app.logger.info('The scrapy process is running on start of the flask app.')
        if scrapyp.process.poll() is not None:
            app.logger.warning('Scrapy process is not running on start of the flask app.')

    @app.before_first_request
    def init_reddit():
        redditp.run()
        if redditp.process.poll() is None:
            app.logger.info('The reddit process is running on start of the flask app.')
        if redditp.process.poll() is not None:
            app.logger.warning('Reddit process is not running on start of the flask app.')

    return app

def extensions(app):
    Bootstrap(app)
    socketio.init_app(app)
    debug_toolbar.init_app(app)
    db.init_app(app)
    #if app.config['WTF_CSRF_ENABLED']:
    csrf.init_app(app)
    security.init_app(app, datastore=user_datastore)

def install_admin(app, db):
    admin.init_app(app)

    # Views for the registered users.
    admin.add_view(SiteModelViewForRegistered(Site, db.session, endpoint='manager_site',
        name='Sites'))
    admin.add_view(TagModelViewForRegistered(Tag, db.session, endpoint='manager_tag',
        name='Tags'))
    # NOTE We could add a view for own suggestions with this solution: https://stackoverflow.com/questions/26349773/flask-admin-default-filters

    # Moderation.
    admin.add_view(ChangeRequestModelView(ChangeRequest, db.session,
        name='Suggestions (M)', category='Moderation'))
    admin.add_view(SiteModelView(Site, db.session, endpoint='manager_site_adm', name='Sites (M)',
        category='Moderation'))
    admin.add_view(TagModelView(Tag, db.session, endpoint='manager_tag_adm', name='Tags (M)',
        category='Moderation'))

    # Scan schedule control.
    admin.add_view(ManagerAdminView(ScanJob, db.session, category='Scans'))
    admin.add_view(ManagerAdminView(ScrapeRequest, db.session, category='Scans'))

    # The true admin level.
    admin.add_view(ManagerAdminView(AppUser, db.session, name='Users'))
    admin.add_view(ManagerAdminView(LiveConfigValue, db.session, name='Conf'))

    return admin

if __name__ == '__main__':
    socketio.run(create_app(), host='0.0.0.0', port=8000)
