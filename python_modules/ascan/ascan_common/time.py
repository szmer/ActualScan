from datetime import datetime, timezone

def date_fmt(time_obj):
    """
    Format the time object to a string formatted for Solr.
    """
    return time_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def timestamp_now():
    return date_fmt(datetime.now(tz=timezone.utc))
