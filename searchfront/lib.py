from datetime import datetime, timezone

def now_time():
    return datetime.now(timezone.utc)
