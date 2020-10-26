from scan.models import Site, Tag

def relevance_table_from_suggestions(suggestions):
    relevance = {} # track if the change postulates are already met
    relevant_tags = { tag.id: tag for tag
            in (Tag.objects.filter(id__in=[sug.target for sug in suggestions
                if sug.record_type == 'tag'])) }
    relevant_sites = { site.id: site for site
            in (Site.objects.filter(id__in=[sug.target for sug in suggestions
                if sug.record_type == 'site']))}
    for suggestion in suggestions:
        relevance[suggestion] = {}
        if suggestion.record_type == 'tag':
            tag = relevant_tags[suggestion.target]
            for field, desired_val in suggestion.suggestion.items():
                relevance[suggestion][field] = (desired_val == getattr(tag, field))
        else: # a site suggestion
            site = relevant_sites[suggestion.target]
            for field, desired_val in suggestion.suggestion.items():
                if field == 'tags':
                    relevance[suggestion][field] = (set(desired_val.split(' '))
                            != set([link.tag.name for link in site.tag_links.all()]))
                else:
                    relevance[suggestion][field] = (desired_val != getattr(site, field))
    return relevance
