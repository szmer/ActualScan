import json
from logging import info
import os

from dynamic_preferences.registries import global_preferences_registry
import pexpect
from redis import Redis
import redis_lock

redis = Redis(host='redis', port=6379, db=0, password=os.environ['REDIS_PASS'])

class OmnivoreError(RuntimeError):
    pass

class OmnivoreBlocked(RuntimeError):
    pass

def date_fmt(time_obj):
    """
    The format liked by Solr.
    """
    return time_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def trust_level_to_numeric(str_level):
    global_preferences = global_preferences_registry.manager()
    return global_preferences['trust_levels__trust_level_threshold_'+str_level]

def numeric_to_trust_level(value):
    global_preferences = global_preferences_registry.manager()
    if value >= global_preferences['trust_levels__trust_level_threshold_base']:
        return 'base'
    if value >= global_preferences['trust_levels__trust_level_threshold_respected']:
        return 'respected'
    if value >= global_preferences['trust_levels__trust_level_threshold_community']:
        return 'community'

def omnivore_call(query_phrase, args='', low_priority=False):
    """
    Args are all the other arguments to pass to the Lisp omnivore runner as string. If low_priority,
    we will give up if the first lock isn't free.
    """
    global_preferences = global_preferences_registry.manager()
    locks_count = global_preferences['index_searching__omnivore_concurrent_jobs']
    timeout = global_preferences['index_searching__omnivore_timeout']
    for lock_n in range(locks_count):
        lock = redis_lock.Lock(redis, 'omnivore-process-{}'.format(lock_n), expire=timeout+3)
        if not lock.acquire():
            if low_priority:
                break
            else:
                continue
        # May signal a pexpect.TIMEOUT.
        run_str = 'sbcl --script /ascan/lisp-startup-omnivore.lisp {} \'{}\''.format(
                    args, query_phrase.replace('\'', '\\\''))
        info('Running omnivore with: {}'.format(run_str))
        (output, status) = pexpect.run(run_str, timeout=timeout, withexitstatus=1)
        if lock.locked():
            lock.release()
        if status == 0:
            return json.loads(output) # may signal a JSONDecodeError
        else:
            raise OmnivoreError('Omnivore error: {}'.format(output))
    raise OmnivoreBlocked
