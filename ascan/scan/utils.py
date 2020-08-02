from dynamic_preferences.registries import global_preferences_registry

def date_fmt(time_obj):
    """
    The format liked by Solr.
    """
    return time_obj.strftime('%Y-%m-%dT%H:%M:%SZ')

def site_level_to_numeric(str_level):
    global_preferences = global_preferences_registry.manager()
    return global_preferences['site_level_threshold_'+str_level]

def tag_level_to_numeric(str_level):
    global_preferences = global_preferences_registry.manager()
    return global_preferences['tag_level_threshold_'+str_level]
