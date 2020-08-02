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

# Debugging

## Debugging containers

You can add to the container options in docker-compose.yml:
    stdin_open: true # docker run -i
    tty: true        # docker run -t

Run the container only with the default command, e.g.
docker-compose run scrapy scrapy crawl general -L DEBUG --pdb

You sometimes can also see logs with:

docker-compose logs <container_name>

## Debugging Postgres

```bash
docker-compose exec postgres psql -U teremin
```

The container must be running for "exec" to work!

Note that `while True` loops in the website container generally produce an unending stream of
transaction, so it may have to be restarted.

## Debugging Scrapy, Reddit scraper

- change the call in scrapy_process.py, reddit_process.py to -L INFO or -L DEBUG
