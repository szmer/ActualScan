from searchfront.blueprints.live_config import LiveConfigValue

def site_level_formatter(view, context, model, name):
    if model.level > int(LiveConfigValue.query.get('site_level_threshold_base').value):
        return 'base'
    elif model.level > int(LiveConfigValue.query.get('site_level_threshold_community').value):
        return 'community'
    return 'flagged'

def tag_level_formatter(view, context, model, name):
    if model.level > int(LiveConfigValue.query.get('tag_level_threshold_base').value):
        return 'base'
    elif model.level > int(LiveConfigValue.query.get('tag_level_threshold_community').value):
        return 'community'
    return 'flagged'

def site_level_formatter_admin(view, context, model, name):
    if model.level > int(LiveConfigValue.query.get('site_level_threshold_base').value):
        return 'base ({})'.format(model.level)
    elif model.level > int(LiveConfigValue.query.get('site_level_threshold_community').value):
        return 'community ({})'.format(model.level)
    return 'flagged ({})'.format(model.level)

def tag_level_formatter_admin(view, context, model, name):
    if model.level > int(LiveConfigValue.query.get('tag_level_threshold_base').value):
        return 'base ({})'.format(model.level)
    elif model.level > int(LiveConfigValue.query.get('tag_level_threshold_community').value):
        return 'community ({})'.format(model.level)
    return 'flagged ({})'.format(model.level)
