from datetime import datetime, timezone

def date_fmt(time_obj):
    """
    format the time object to a string formatted for solr.
    """
    return time_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def time_now():
    return datetime.now(tz=timezone.utc)
