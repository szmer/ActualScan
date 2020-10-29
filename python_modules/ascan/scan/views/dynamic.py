from datetime import datetime, timezone
from logging import debug, info
import random
import socket

from django.conf import settings
from django.contrib import messages
from django.shortcuts import redirect, render
from django.views.decorators.csrf import csrf_exempt
from dynamic_preferences.registries import global_preferences_registry
from ipware import get_client_ip

from bg.models import QueryRecord
from scan.control import (maybe_issue_guest_scan_permission, request_scan, scan_progress_info,
        terminate_scan, verify_scan_permission, maybe_give_feedback_tag_site_link)
from scan.forms import PublicScanForm, EditableScanForm
from scan.get_results import rules_results
from scan.utils import trust_level_to_numeric, numeric_to_trust_level
from scan.models import ScanJob, Site, TagSiteLink, ResultRule

def scaninfo(request):
    if not 'job_id' in request.GET:
        messages.add_message(request, messages.ERROR, 'No scan job specified.')
        context = { 'status_data': { 'phase': 'Unknown job.' }, 'result': False }
        return render(request, 'scan/scaninfo.html', context=context, status=400)
    job_id = request.GET['job_id']
    try:
        job = ScanJob.objects.get(id=job_id)
    except ScanJob.DoesNotExist:
        job = None
    print(request.user.is_staff, request.user)
    if job is None or not (request.user.is_staff or request.user == job.user):
        messages.add_message(request, messages.ERROR, 'Bad scan job specified.')
        context = { 'status_data': { 'phase': 'Bad job.' }, 'result': False }
        return render(request, 'scan/scaninfo.html', context=context, status=400)
    if 'terminate' in request.GET and request.GET['terminate']:
        terminate_scan(job.id)
    progress_info = scan_progress_info(job.id)
    index_form_data = { 'query_tag': job.query_tag }
    index_form_data['scan_query'] = job.query_phrase
    index_form_data['query_sites'] = job.query_site_names
    index_form_data['minimal_level'] = numeric_to_trust_level(job.minimal_level)
    index_form = PublicScanForm(data=index_form_data)
    return render(request, 'scan/scaninfo.html',
            { 'status_data': progress_info, 'terminable': job.status in ['waiting', 'working'],
                'site_names': job.query_site_names.strip().split(','),
                'tag_names': job.query_tags.strip().split(','),
                'scan_job': job, 'form': index_form })

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
            return redirect('/scan/scaninfo/?job_id={}'.format(job.id))
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

    context['tabs'] = ResultRule.objects.all()
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
        context.update(rules_results(scan_query, rules, query_site_names=query_site_names))
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
