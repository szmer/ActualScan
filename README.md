# Structure

# Testing

```bash
docker-compose exec website pytest --capture=sys searchfront
```

To skip tests requiring many network connections:

```bash
docker-compose exec website pytest -k "not with_network" searchfront
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

## Debugging Scrapy, Reddit scraper

- change the call in scrapy_process.py, reddit_process.py to -L INFO or -L DEBUG
