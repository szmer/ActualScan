from flask import Flask

from searchfront.blueprints.page import page

def create_app(settings_override=None):
    app = Flask(__name__, instance_path='/flask_instance')

    app.config.from_object('flask_config.settings')
    # Overwrite with the settings from the instance_path set above
    app.config.from_pyfile('settings.py', silent=True)

    if settings_override:
        app.config.update(settings_override)

    app.register_blueprint(page)
    extensions(app)

    return app

def extensions(app):
    pass
