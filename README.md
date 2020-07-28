# Structure

Flask-Admin templates are partly overwritten in ModelViews and live inside `templates/manager`.
Overwritten Flask-Security templates are inside the account blueprint, inside `templates/security`.

# Development enviroment.

Testing currently creates some example sites and tags.

# Killing all containers

```bash
docker kill $(docker ps -q)
```

# Running migrations

```bash
docker-compose run website python manage.py makemigrations
docker-compose run website python manage.py migrate auth
docker-compose run website python manage.py migrate scan xxxx
docker-compose run website python manage.py migrate
```

# Testing

```bash
docker-compose build website
docker-compose run website python manage.py test scan
```

# Installing frontend packages

- add to `package.json` in the webpack directory
- run `npm install` (on the local machine)
- add imports in `app/app.scss`, `app/app.js`
- run `yarnpkg build`

# Debugging

You can use the `--capture=sys` flag in invoking pytest to print stdout/stderr prints, also from
underlying processes (scrapyp, redditp).

For problems with pytest:

```bash
# Overview of what the pytest would do.
docker-compose exec website pytest -m trace --trace searchfront
docker-compose exec website pytest -v
```

## Debugging Flask

Use this for accessing the main app logger from blueprints.

```python
from flask import current_app
current_app.logger.info('hey')
```

## Debugging Postgres

```bash
docker-compose exec postgres psql -U teremin
```

Note that `while True` loops in the website container generally produce an unending stream of
transaction, so it may have to be restarted.

## Debugging Scrapy, Reddit scraper

- change the call in scrapy_process.py, reddit_process.py to -L INFO or -L DEBUG
