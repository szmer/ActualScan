from flask import Flask
from flask_security.utils import encrypt_password

from searchfront.extensions import db, security, user_datastore, admin
from searchfront.scrapy_process import scrapyp

# Import blueprints now as some of them may require setting up extensions beforehand.
from searchfront.blueprints.page import page
from searchfront.blueprints.user import AppUser
from searchfront.blueprints.manager import ManagerAdminView
from searchfront.blueprints.site.models import Site
from searchfront.blueprints.scan_schedule.models import ScrapRequest

def create_app(settings_override=None):
    app = Flask(__name__, instance_path='/flask_instance', instance_relative_config=True)

    app.config.from_object('flask_config.settings')
    # Overwrite with the settings from the instance_path set above
    app.config.from_pyfile('settings.py', silent=True)

    if settings_override:
        app.config.update(settings_override)

    app.register_blueprint(page)
    extensions(app)

    @app.before_first_request
    def init_db():
        with app.app_context():
            db.create_all()

            # Create the roles.
            user_datastore.find_or_create_role(name='admin', description='Administrator')
            user_datastore.find_or_create_role(name='registered', description='Registered user')
            db.session.commit()

            # Add the root user.
            if not user_datastore.get_user(app.config['INIT_USER_EMAIL']):
                user_datastore.create_user(email=app.config['INIT_USER_EMAIL'],
                        password=encrypt_password(app.config['INIT_USER_PASSWORD']))
            db.session.commit()
            user_datastore.add_role_to_user(app.config['INIT_USER_EMAIL'], 'admin')
            db.session.commit()

    @app.before_first_request
    def init_scrapy():
        scrapyp.run()
        if scrapyp.process.poll() is None:
            app.logger.info('The scrapy process is running on start of the flask app.')
        if scrapyp.process.poll() is not None:
            app.logger.warning('Scrapy process is not running on start of the flask app.')

    admin.add_view(ManagerAdminView(AppUser, db.session))

    return app

def extensions(app):
    db.init_app(app)
    security.init_app(app, datastore=user_datastore)
    admin.init_app(app)
