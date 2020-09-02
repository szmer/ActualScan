from logging import debug, info
import random

from django.contrib import messages
from django.contrib.auth.decorators import login_required
from django.shortcuts import redirect, render
from dynamic_preferences.registries import global_preferences_registry
from ipware import get_client_ip
import pexpect

from scan.control import (maybe_issue_guest_scan_permission, request_scan, scan_progress_info,
        terminate_scan, verify_scan_permission, maybe_issue_feedback_permission)
from scan.forms import PublicScanForm
from scan.utils import date_fmt, trust_level_to_numeric, omnivore_call, OmnivoreError, OmnivoreBlocked
from scan.models import ScanJob, Site, TagSiteLink

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

def scaninfo(request):
    if not 'job_id' in request.GET:
        messages.add_message(request, messages.ERROR, 'No scan job specified.')
        context = { 'status_data': { 'phase': 'Unknown job.' }, 'result': False }
        return render(request, 'scan/scaninfo.html', context=context, status=400)
    job_id = request.GET['job_id']
    job = ScanJob.objects.get(id=job_id)
    if job is None or not (request.user.is_staff or request.user == job.user):
        messages.add_message(request, messages.ERROR, 'Bad scan job specified.')
        context = { 'status_data': { 'phase': 'Bad job.' }, 'result': False }
        return render(request, 'scan/scaninfo.html', context=context, status=400)
    if 'terminate' in request.GET and request.GET['terminate']:
        terminate_scan(job.id)
    progress_info = scan_progress_info(job.id)
    return render(request, 'scan/scaninfo.html',
            { 'status_data': progress_info, 'terminable': job.status in ['waiting', 'working'],
                'site_names': job.query_site_names.strip().split(','),
                'tag_names': job.query_tags.strip().split(','),
                'scan_job': job })

def search(request):
    global_preferences = global_preferences_registry.manager()

    # Parse the form information to know what to search for.
    form = PublicScanForm(data=request.GET)
    if not form.is_valid():
        context = { 'with_errors': True, 'form': form }
        return render(request, 'scan/indexresults.html', context=context, status=400)

    # Unpacking the form data.
    scan_query = form.cleaned_data['scan_query']
    query_tags = [tag.name for tag in form.cleaned_data['query_tags']]
    query_site_names = [site.site_name for site in form.cleaned_data['query_sites']]
    start_date = date_fmt(form.cleaned_data['start_date'])
    end_date = date_fmt(form.cleaned_data['end_date'])
    allow_undated = form.cleaned_data['allow_undated']
    minimal_level = trust_level_to_numeric(form.cleaned_data['minimal_level'])
    debug('query tags: {}, minimal level: {}'.format(query_tags, minimal_level))
    debug('start time: {}, end time: {}'.format(start_date, end_date))

    # Is an actual scan requested?
    if 'is_scan' in request.GET and request.GET['is_scan']:
        scan_performed = verify_scan_permission(request.user, get_client_ip(request)[0])
        # If the scan is being performed, check if it is finished and display progress information
        # if it's not.
        if scan_performed:
            debug('A scan will be performed.')
            job = request_scan((request.user if request.user.is_authenticated
                else get_client_ip(request)),
                scan_query, query_tags=query_tags, query_site_names=query_site_names,
                minimal_level=minimal_level,
                start_date=form.cleaned_data['start_date'],
                end_date=form.cleaned_data['end_date'],
                allow_undated=allow_undated,
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
    query_site_names += [site.site_name
                for site in Site.objects.filter(
                    tag_links__tag__in=form.cleaned_data['query_tags']).all()]
    context = { 'scan_phrase': scan_query }
    try:
        omnivore_results = omnivore_call(scan_query, args=
                '--start-date {} --end-date {} {} {}'.format(
                    start_date, end_date,
                    '--undated' if allow_undated else '',
                    '--sites '+' '.join(query_site_names) if query_site_names else ''))
    except (pexpect.TIMEOUT, OmnivoreError,  OmnivoreBlocked) as e:
        info('Exception {} blocked getting a result from omnivore'.format(type(e).__name__))
        messages.add_message(request, messages.ERROR, 'Our server seems to be overloaded now.')
        context['with_errors'] = True
        return render(request, 'scan/indexresults.html', context=context, status=503)
    info('Omnivore responded for {}: {}'.format(scan_query, omnivore_results))
    context['result'] = omnivore_results

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

    return render(request, 'scan/indexresults.html', context=context)
