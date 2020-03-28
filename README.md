# Structure

# Testing

```bash
docker-compose exec website pytest --capture=sys searchfront
```

# Debugging

You can use the `--capture=sys` flag in invoking pytest to print stdout/stderr prints, also from
underlying processes (scrapyp, redditp).

## Debugging Postgres

```bash
docker-compose exec postgres psql -U teremin
```

## Debugging Scrapy, Reddit scraper

- change the call in scrapy_process.py, reddit_process.py to -L INFO or -L DEBUG
