# Setup

```
docker-compose build
docker-compose run website python manage.py makemigrations scan
docker-compose run website python manage.py makemigrations manager
docker-compose run website python manage.py makemigrations bg
docker-compose run website python manage.py migrate
docker-compose up --build
```

## Creating SSL certs for internal communications

```
cd certs/dev # or replace dev with prod, also everywhere below:
openssl req -x509 -newkey rsa:4096 -keyout ascan_dev_internal.pem -out ascan_dev_internal.pem -days 365 -config ./openssl.cnf
openssl pkcs12 -export -in ascan_dev_internal.pem -inkey ascan_dev_internal.pem -out ascan_dev_ssl_internal.keystore.p12 -name "ascan-solr"
```
All passwords that you enter should be consistent with `KEYSTORE_PASSWORD` variable in your `.env` file.

# Running migrations

NOTE on docker-compose you need to those manually (to allow you to delay migrations in development),
ansible script will always migrate for you automatically
```bash
docker-compose exec website python manage.py makemigrations
docker-compose exec website python manage.py migrate auth
docker-compose exec website python manage.py migrate scan xxxx
docker-compose exec website python manage.py migrate
```

# Testing

```bash
bash run-tests-docker-compose.sh # run everything
docker-compose exec website pytest -x --pdb # run one container
```

# Debugging

## Debugging containers

You can add to the container options in docker-compose.yml:
    stdin_open: true # docker run -i
    tty: true        # docker run -t

!! Run the container only with the default command, e.g.
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

# Solr cores

docker-compose exec solr solr create_core -c ascan -d server/solr/configsets/ascan
docker-compose exec solr solr delete -c ascan
