import http.client
import json
from logging import debug, info
import random
from urllib.parse import quote

from django.contrib import messages
from django.contrib.auth.decorators import login_required
from django.shortcuts import render
from dynamic_preferences.registries import global_preferences_registry
from ipware import get_client_ip

from scan.control import (maybe_issue_guest_scan_permission, request_scan, scan_progress_info,
        verify_scan_permission, maybe_issue_feedback_permission)
from scan.forms import PublicScanForm
from scan.utils import date_fmt, trust_level_to_numeric
from scan.models import TagSiteLink

@login_required
def accountinfo(request):
    return render(request, 'registration/info.html')

def index(request):
    form = PublicScanForm()
    # TODO KLUDGE currently unlimited scan permissions for registered
    if request.user.is_authenticated:
        can_scan = True
    else:
        can_scan = maybe_issue_guest_scan_permission(get_client_ip(request))
    return render(request, 'scan/home.html', context={ 'can_scan': can_scan, 'form': form })

def scanresults(request):
    global_preferences = global_preferences_registry.manager()

    # Parse the form information to know what to search for.
    form = PublicScanForm(data=request.GET)
    if not form.is_valid():
        # Make a short dict compatible with what scan_progress_info would return.
        context = { 'status_data': { 'phase': 'Bad request failure.' },
                'errors': form.errors, 'is_good': False, 'result': False }
        return render(request, 'scan/scanresults.html', context=context, status=400)

    # Unpacking the form data.
    scan_query = form.cleaned_data['scan_query']
    query_tags = [tag.name for tag in form.cleaned_data['query_tags']]
    start_date = date_fmt(form.cleaned_data['start_date'])
    end_date = date_fmt(form.cleaned_data['end_date'])
    allow_undated = form.cleaned_data['allow_undated']
    minimal_level = trust_level_to_numeric(form.cleaned_data['minimal_level'])
    debug('query tags: {}, minimal level: {}'.format(query_tags, minimal_level))
    debug('start time: {}, end time: {}'.format(start_date, end_date))

    # Is an actual scan requested?
    if 'is_scan' in request.GET and request.GET['is_scan']:
        scan_performed = verify_scan_permission(request.user, get_client_ip(request)[0])
        scan_finished = False
        # If the scan is being performed, check if it is finished and display progress information
        # if it's not.
        if scan_performed:
            debug('A scan will be performed.')
            job = request_scan((request.user if request.user.is_authenticated
                else get_client_ip(request)),
                scan_query, query_tags,
                minimal_level=minimal_level,
                is_ip=not request.user.is_authenticated,
                is_privileged=request.user.is_staff)
            progress_info = scan_progress_info(job.id)
            if ('phase' in progress_info and progress_info['phase'] == 'finished'):
                scan_finished = True
            if not scan_finished:
                return render(request, 'scan/scanresults.html',
                        { 'status_data': progress_info,
                            'scan_phrase': scan_query, 'scan_id': job.id,
                            'is_good': ('phase' in progress_info) and (progress_info['phase']
                                not in ['rejected', 'terminated'])})
        # If the scan request just failed, flash the negative infomation.
        # TODO redirect back in a UX fashion
        if not scan_performed and not scan_finished:
            debug('A scan was requested and rejected.')
            messages.add_message(request, messages.WARNING,
                    'Sorry! We can\'t currently give you the resources to scan. Here\'s the index '
                    'results instead.')

    # The index-only search response (if there is no scan or it finished).
    try:
        omnivore_conn = http.client.HTTPConnection('omnivore', port=4242, timeout=10)
        # TODO query tags (give Solr the acceptable sites)
        omnivore_conn.request('GET', '/result?q={}&sdate={}&edate={}&und={}'.format(
            quote(scan_query), start_date, end_date, '1' if allow_undated else '0'),
            headers={'Content-type': 'application/json'})
        omnivore_response = omnivore_conn.getresponse()
        omnivore_results = json.loads(omnivore_response.read())
        debug('Omnivore responded for {}: {}'.format(scan_query,
            omnivore_results))
        context = { 'result': omnivore_results, 'scan_phrase': scan_query, 'is_good': True }

        # Try to issue and add a feedback permission.
        if (omnivore_results['sitesStats'] is not None
                and random.random() < global_preferences['feedback_ask_frequency']):
            debug('Trying to issue a feedback permission...')
            site_names = [item['site'] for item in omnivore_results['sitesStats']]
            gradable_links = TagSiteLink.objects.filter(tag__name__in=query_tags,
                    site__site_name__in=site_names, level__gte=minimal_level).all()
            debug('{} gradable tag-site links.'.format(len(gradable_links)))
            gradable_link = maybe_issue_feedback_permission(
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

        return render(request, 'scan/scanresults.html', context=context)
    except Exception as e:
        info('Error processing scan/search request: {}'.format(e))
        context = { 'status_data': { 'phase': 'Internal error when processing the request.' },
                'is_good': False, 'result': False }
        return render(request, 'scan/scanresults.html', context=context, status=500)
