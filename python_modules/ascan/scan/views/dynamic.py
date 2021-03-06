from datetime import datetime, timezone
from logging import debug, info
import random
import socket

from django.conf import settings
from django.contrib import messages
from django.contrib.admin.views.decorators import staff_member_required
from django.shortcuts import redirect, render
from django.views.decorators.csrf import csrf_exempt
from dynamic_preferences.registries import global_preferences_registry
from ipware import get_client_ip

from bg.models import QueryRecord
from scan.control import (maybe_issue_guest_scan_permission, request_scan, verify_scan_permission,
        maybe_give_feedback_tag_site_link)
from scan.forms import EditableScanForm
from scan.get_results import rules_results
from scan.utils import trust_level_to_numeric
from scan.models import Site, TagSiteLink, ResultRule
from manager.forms import SimpleCrawlForm

@staff_member_required
def crawl(request):
    form = SimpleCrawlForm(request.POST)
    context = { 'form': form }
    if form.is_valid():
        print(form.data)
        job = request_scan(request.user, '', query_tags=[],
                query_site_names=[form.cleaned_data['crawl_site'].site_name],
                is_simple_crawl=True,
                is_ip=False, is_privileged=True)
        context['start_ok'] = True
        context['job_id'] = job.id
        context['site_name'] = form.cleaned_data['crawl_site'].site_name
    return render(request, 'scan/crawl.html', context=context)

@csrf_exempt
def search(request):
    global_preferences = global_preferences_registry.manager()

    # Parse the form information to know what to search for.
    form = EditableScanForm(data=request.GET)
    if not form.is_valid():
        context = { 'with_errors': True, 'form': form }
        return render(request, 'scan/indexresults.html', context=context, status=400)

    # Unpacking the form data.
    scan_query = form.cleaned_data['scan_query']
    query_tags = [tag.name for tag in form.cleaned_data['query_tags']]
    explicit_site_names = [site.site_name for site in form.cleaned_data['query_sites']]
    minimal_level = trust_level_to_numeric(form.cleaned_data['minimal_level'])
    debug('query tags: {}, minimal level: {}'.format(query_tags, minimal_level))

    # Is an actual scan requested?
    if 'is_scan' in request.GET and request.GET['is_scan']:
        scan_performed = verify_scan_permission(request.user, get_client_ip(request)[0])
        # If the scan is being performed, check if it is finished and display progress information
        # if it's not.
        if scan_performed:
            debug('A scan will be performed.')
            job = request_scan((request.user if request.user.is_authenticated
                else get_client_ip(request)),
                scan_query, query_tags=query_tags, query_site_names=explicit_site_names,
                minimal_level=minimal_level,
                is_ip=not request.user.is_authenticated,
                is_privileged=request.user.is_staff)
            return redirect('/manager/scaninfo/?job_id={}'.format(job.id))
        # If the scan request just failed, flash the negative infomation.
        if not scan_performed:
            debug('A scan was requested and rejected.')
            messages.add_message(request, messages.WARNING,
                    'Sorry! We can\'t currently give you the resources to scan. Here\'s the index '
                    'results instead.')

    # The index-only search response (if there is no scan or it finished).
    # TODO KLUDGE currently unlimited scan permissions for registered
    if request.user.is_authenticated:
        can_scan = True
    else:
        can_scan = maybe_issue_guest_scan_permission(get_client_ip(request))
    query_site_names = explicit_site_names + [site.site_name
            for site in Site.objects.filter(
                tag_links__tag__in=form.cleaned_data['query_tags']).all()]
    context = { 'scan_phrase': scan_query, 'form': form, 'can_scan': can_scan,
            'sites': query_site_names, 'tags': query_tags }

    context['tabs'] = ResultRule.objects.filter(assigned_user_id__in=[-1]
            +([request.user.id] if request.user.is_authenticated else []))
    rules = ''
    if form.cleaned_data['rule']:
        rules = form.cleaned_data['rule']
        context['current_tab'] = settings.DEFAULT_RESULT_RULE['slug']
    elif form.cleaned_data['tab']:
        for rule in context['tabs']:
            if rule.slug == form.cleaned_data['tab']:
                context['current_tab'] = rule.slug
                rules = rule.rule_string
    else:
        context['current_tab'] = settings.DEFAULT_RESULT_RULE['slug']
        rules = settings.DEFAULT_RESULT_RULE['rule_string']

    try:
        context.update(rules_results(scan_query, rules, query_site_names=query_site_names,
            highlight=True))
    except socket.timeout:
        messages.add_message(request, messages.ERROR, 'Our server seems to be overloaded now.')
        context['with_errors'] = True
        return render(request, 'scan/indexresults.html', context=context, status=503)
    except Exception as e:
        info('Exception {} blocked getting a result from Solr: {}'.format(type(e).__name__, e))
        messages.add_message(request, messages.ERROR, 'We experienced an internal error.')
        context['with_errors'] = True
        return render(request, 'scan/indexresults.html', context=context, status=503)

    # Try to issue and add a feedback permission.
    if (random.random() < global_preferences['trust_levels__feedback_ask_frequency']):
        debug('Trying to issue a feedback permission...')
        site_names = list(set(doc['site_name'] for doc in context['result']))
        gradable_links = TagSiteLink.objects.filter(tag__name__in=query_tags,
                site__site_name__in=site_names, level__gte=minimal_level).all()
        debug('{} gradable tag-site links.'.format(len(gradable_links)))
        gradable_link = maybe_give_feedback_tag_site_link(
                (request.user if request.user.is_authenticated
                    else get_client_ip(request)),
                gradable_links,
                is_ip=not request.user.is_authenticated)
        debug('Gradable link maybe: {}'.format(gradable_link))
        if gradable_link:
            info('Issued permission for feedback on {} for {}/{}'.format(
                gradable_link, request.user, get_client_ip(request)))
            context['possible_feedback'] = {
                    'site': gradable_link.site.site_name,
                    'tag': gradable_link.tag.name
                    }

    # Make the query record if needed.
    if request.user.is_authenticated and request.user.userprofile.wants_query_records:
        try: # don't duplicate if possible
            existing_record = QueryRecord.objects.get(person=request.user, query_phrase=scan_query,
                    query_site_names=explicit_site_names, query_tags=query_tags,
                    minimal_level=form.cleaned_data['minimal_level'])
            existing_record.date_submitted = datetime.now(tz=timezone.utc)
            existing_record.save()
        except QueryRecord.DoesNotExist:
            QueryRecord.objects.create(person=request.user, query_phrase=scan_query,
                    query_site_names=explicit_site_names, query_tags=query_tags,
                    minimal_level=form.cleaned_data['minimal_level'])

    return render(request, 'scan/indexresults.html', context=context)
