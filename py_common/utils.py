from urllib.parse import urlparse

def full_url(partial_url, site_url):
    """
    If the url lacks the domain or scheme field, fill it in using the site url.
    """
    partial_parsed = urlparse(partial_url)
    if partial_parsed.netloc and partial_parsed.scheme:
        return partial_url
    site_url_parsed = urlparse(site_url)
    partial_parsed = partial_parsed._replace(netloc=site_url_parsed.netloc)
    partial_parsed = partial_parsed._replace(scheme=site_url_parsed.scheme)
    return partial_parsed.geturl()

def write_to_file(path, content):
    with open(path, 'w+') as out_file:
        print(content, file=out_file)

def update_request_status(request, new_status, failure_comment=None):
    """
    Update the status of an ascan ScrapeRequest.
    """
    if failure_comment is not None:
        request.failure_comment = failure_comment
    request.change_status(new_status) # this should also save()
