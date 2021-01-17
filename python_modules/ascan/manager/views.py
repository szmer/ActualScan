from datetime import datetime, timedelta, timezone
from logging import info, warning
import re
from urllib.parse import urlparse
import urllib.request

from django.core.validators import URLValidator
from django.core.exceptions import ValidationError
from django.contrib import messages
from django.contrib.admin.views.decorators import staff_member_required
from django.contrib.auth.decorators import login_required
from django.db import Error, IntegrityError
from django.http import HttpResponse
from django.shortcuts import get_object_or_404, redirect, render
from django.views.generic import ListView
from dynamic_preferences.registries import global_preferences_registry

from manager.forms import (
        SiteForm, TagForm, EditRequestSiteForm, EditRequestTagForm, BlocklistForm, SimpleCrawlForm,
        ResultRuleForm
        )
from manager.models import EditSuggestion, BlockedSite, EDIT_SUGGESTION_STATUSES
from manager.utils import relevance_table_from_suggestions
from scan.control import scan_progress_info, terminate_scan
from scan.forms import PublicScanForm
from scan.models import Site, Tag, TagSiteLink, ScanJob, ScrapeRequest, ResultRule
from scan.templatetags.scan_extras import format_trust_level
from scan.utils import trust_level_to_numeric, numeric_to_trust_level

class HomeURLParsingError(Error):
    pass

class SearchURLParsingError(Error):
    pass

class SiteList(ListView):
    model = Site
    context_object_name = 'sites'
    paginate_by = 18

class SiteSearchList(ListView):
    model = Site
    context_object_name = 'sites'
    paginate_by = 18

    template_name = 'scan/site_list.html'

    def get_queryset(self):
        return Site.objects.filter(site_name__contains=self.request.GET['q'])

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['q'] = self.request.GET['q']
        return context

class TagList(ListView):
    model = Tag
    context_object_name = 'tags'
    paginate_by = 18

class TagSearchList(ListView):
    model = Tag
    context_object_name = 'tags'
    paginate_by = 18

    template_name = 'scan/tag_list.html'

    def get_queryset(self):
        return Tag.objects.filter(name__contains=self.request.GET['q'])

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['q'] = self.request.GET['q']
        return context

def tag_details(request, pk):
    tag = Tag.objects.get(id=pk)
    form = EditRequestTagForm(instance=tag)
    suggestions = EditSuggestion.objects.filter(
            record_type='tag', target=tag.id).order_by('-date_submitted')
    context = { 'tag': tag, 'form': form, 'suggestions': suggestions }
    return render(request, 'scan/tag_detail.html', context=context)

def site_details(request, pk):
    site = Site.objects.get(id=pk)
    form = EditRequestSiteForm(instance=site,
            initial={'tags': [link.tag for link in site.tag_links.all()]})
    suggestions = EditSuggestion.objects.filter(
            record_type='site', target=site.id).order_by('-date_submitted')
    context = { 'site': site, 'form': form, 'suggestions': suggestions }
    return render(request, 'scan/site_detail.html', context=context)

def tagname(request, tag_name):
    """
    Look up the tag by name instead of the primary key.
    """
    tag = get_object_or_404(Tag, name=tag_name)
    return render(request, 'scan/tag_detail.html', { 'tag': tag })

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
    if job is None or not (request.user.is_staff or request.user == job.user):
        messages.add_message(request, messages.ERROR, 'Bad scan job specified.')
        context = { 'status_data': { 'phase': 'Bad job.' }, 'result': False }
        return render(request, 'scan/scaninfo.html', context=context, status=400)
    if 'terminate' in request.GET and request.GET['terminate']:
        terminate_scan(job.id)
    progress_info = scan_progress_info(job.id)
    index_form_data = { 'query_tags': job.query_tags }
    index_form_data['scan_query'] = job.query_phrase
    index_form_data['query_sites'] = job.query_site_names
    index_form_data['minimal_level'] = numeric_to_trust_level(job.minimal_level)
    index_form = PublicScanForm(data=index_form_data)
    return render(request, 'manager/scaninfo.html',
            { 'status_data': progress_info, 'terminable': job.status in ['waiting', 'working', 'finished'],
                'site_names': job.query_site_names,
                'tag_names': job.query_tags,
                'scan_job': job, 'form': index_form })

def scans(request):
    jobs = ScanJob.objects.all().order_by('-status_changed')
    page_counts = []
    for job in jobs:
        page_counts.append(ScrapeRequest.objects.filter(job=job).count())
    context = { 'scans': zip(jobs, page_counts) }
    if request.user.is_staff:
        context['form'] = SimpleCrawlForm()
    return render(request, 'manager/scans.html', context)

def tagsites(request, tag_name):
    tag_site_links = get_object_or_404(Tag, name=tag_name).site_links.all()
    context = dict([(label, []) for label in ['base_sites', 'respected_sites', 'community_sites',
        'spam_sites']])
    context['tag'] = tag_name
    for site_link in tag_site_links:
        trust_label = format_trust_level(site_link.level)
        context[trust_label+'_sites'].append(site_link.site)
    return render(request, 'manager/tagsites.html', context=context)

@login_required
def makesite(request):
    submitted = False
    global_preferences = global_preferences_registry.manager()
    day_threshold = datetime.now(timezone.utc) - timedelta(days=1)
    last_sites_count = Site.objects.filter(creator=request.user,
            time_created__gte=day_threshold).count()
    if last_sites_count >= global_preferences['user_permissions__max_sites_per_day']:
        messages.add_message(request, messages.ERROR,
                'Our abuse filter blocked adding more sites today. Please try again tomorrow.')
        site_form = SiteForm()
    elif request.method == 'POST':
        submitted = True
        site_form = SiteForm(request.POST)
        if site_form.is_valid():
            try:
                # Derive the site name and type.
                r_position = site_form.instance.homepage_url.find('/r/')
                # For reddit, the name is /r/+the subreddit name. The user can submit only the /r/
                # part without the full URL.
                if r_position != -1 and (
                        re.search('^[^/]*(//)?[^/]*\\.reddit\\.com/',
                            site_form.instance.search_pointer)
                        or re.search('^reddit\\.com',
                            site_form.instance.search_pointer)
                        or r_position == 0):
                    site_form.instance.site_type = 'reddit'
                    # Where does the subreddit name end.
                    r_end = site_form.instance.homepage_url[r_position+len('/r/'):].find('/')
                    if r_end != -1:
                        # do skip the ending slash, that's how the sites are indexed in Solr
                        r_end = len(site_form.instance.homepage_url[r_position:]) - 1
                    site_form.instance.site_name = site_form.instance.homepage_url[r_position:r_end]
                    site_form.instance.search_pointer = site_form.instance.site_name[len('/r/'):]
                    site_form.instance.homepage_url = ('https://reddit.com'
                            + site_form.instance.homepage_url[r_position:r_end])
                    site_form.instance.source_type = 'forums' # regardless of the user's input
                else:
                    if not site_form.instance.homepage_url.startswith('http'):
                        site_form.instance.homepage_url = 'http://' + site_form.instance.homepage_url
                    if not site_form.instance.search_pointer.startswith('http'):
                        site_form.instance.search_pointer = ('http://'
                                + site_form.instance.search_pointer)
                    validator = URLValidator()
                    # Test that the search pointer is a valid URL.
                    try:
                        validator(site_form.instance.search_pointer)
                        parsed_search_url = urlparse(site_form.instance.search_pointer)
                    except ValidationError as e:
                        raise SearchURLParsingError(site_form.instance.search_pointer, *e.args)
                    except ValueError as e:
                        raise SearchURLParsingError(site_form.instance.search_pointer, *e.args)
                    url_later_part = (parsed_search_url.path+parsed_search_url.params
                            +parsed_search_url.query+parsed_search_url.fragment)
                    if not (Site.MOCK_STR1 in url_later_part and Site.MOCK_STR2 in url_later_part):
                        raise SearchURLParsingError('no mock strs', site_form.instance.search_pointer)
                    # Test and parse the homepage URL.
                    try:
                        validator(site_form.instance.homepage_url)
                        parsed_url = urlparse(site_form.instance.homepage_url)
                    except ValidationError as e:
                        raise HomeURLParsingError(site_form.instance.homepage_url, *e.args)
                    except ValueError as e:
                        raise HomeURLParsingError(site_form.instance.homepage_url, *e.args)
                    site_name = re.sub('^www\\.', '', parsed_url.netloc)
                    # Check for search pointer-homepage mismatches and blocked sites.
                    if not site_name in parsed_search_url.netloc:
                        messages.add_message(request, messages.ERROR,
                                'The homepage and search URLs seem to be mismatched.')
                        raise ValueError('mismatched {} and search {}'.format(
                            site_name, parsed_search_url.netloc))
                    block_entries = BlockedSite.objects.filter(address__contains=site_name).all()
                    if block_entries:
                        messages.add_message(request, messages.ERROR,
                                'This site is blocked due to the content type: {}. There is an '
                                'automatic list, please contact us if this is a mistake.'.format(
                                    block_entries[0].kind))
                        raise ValueError('blocked '+site_name)
                    site_form.instance.site_type = 'web'
                    site_form.instance.site_name = site_name

                # Set the creator field.
                site_form.instance.creator = request.user

                site_obj = site_form.save()
                info(site_obj)

                # Make the appropriate tag links.
                try:
                    for tag in site_form.cleaned_data['tags']:
                        link = TagSiteLink.objects.create(site=site_obj, tag=tag)
                        link.save()
                except Error as e:
                    info(e)
                    messages.add_message(request, messages.ERROR, 'Some tags may no longer exist.')

                messages.add_message(request, messages.SUCCESS, 'The site {} has been added.'.
                        format(site_form.instance.site_name))

                # Return to the list of sites.
                return redirect('sites')
            except HomeURLParsingError as e:
                info(e)
                messages.add_message(request, messages.ERROR, 'We couldn\'t enter the address that'
                        ' you\'ve provided: {}'.format(site_form.instance.homepage_url))
            except SearchURLParsingError as e:
                info(e)
                messages.add_message(request, messages.ERROR, 'We couldn\'t use the search address'
                        ' that you\'ve provided: {}'.format(site_form.instance.search_pointer))
            except IntegrityError as e:
                info(e)
                messages.add_message(request, messages.ERROR, 'Some site data seems to duplicate '
                        'what we already have.')
            except ValueError as e:
                info(e)
                messages.add_message(request, messages.ERROR, 'There was a technical error with the'
                        ' form.')
    else:
        site_form = SiteForm()
    context = { 'form': site_form, 'site_form_submitted': submitted }
    return render(request, 'manager/makesite.html', context)

@login_required
def maketag(request):
    global_preferences = global_preferences_registry.manager()
    day_threshold = datetime.now(timezone.utc) - timedelta(days=1)
    last_tags_count = Tag.objects.filter(creator=request.user,
            time_created__gte=day_threshold).count()
    if last_tags_count >= global_preferences['user_permissions__max_tags_per_day']:
        messages.add_message(request, messages.ERROR,
                'Our abuse filter blocked adding more tags today. Please try again tomorrow.')
        tag_form = TagForm()
    elif request.method == 'POST':
        tag_form = TagForm(request.POST)
        if tag_form.is_valid():
            tag_form.instance.creator = request.user
            tag_form.save()
            messages.add_message(request, messages.SUCCESS, 'The tag {} has been added.'.
                    format(tag_form.instance.name))
            # Return to the list of tags.
            return redirect('tags')
    else:
        tag_form = TagForm()
    context = { 'form': tag_form }
    return render(request, 'manager/maketag.html', context)

@login_required
def suggest(request):
    """
    The user is sent here when submitting a form from a Site or Tag details page.
    The view expects record_type (site, tag) and target (i.e. the name of the object) GET args.
    """
    context = { 'record_type': request.GET['record_type'], 'target_id': request.GET['target_id'] }
    suggestion_dict = {} # we'll fill it only with the values that are changed
    form_ok = False
    error_cause = ''
    # Process possible suggestion types.
    if request.GET['record_type'] == 'site':
        form = EditRequestSiteForm(data=request.GET)
        context['form'] = form
        if form.is_valid():
            try:
                site = Site.objects.get(id=form.cleaned_data['target_id'])
                form_ok = True
                context['target'] = site.site_name
                for field in ['site_type', 'source_type', 'homepage_url', 'search_pointer']:
                    if form.cleaned_data[field] != getattr(site, field):
                        # This field is hidden and non-suggestable for reddit sites.
                        if not (field == 'search_pointer' and site.site_type == 'reddit'):
                            suggestion_dict[field] = form.cleaned_data[field]
                if set(form.cleaned_data['tags']) != set([link.tag for link in site.tag_links.all()]):
                    suggestion_dict['tags'] = ' '.join([tag.name for tag in form.cleaned_data['tags']])
            except Site.DoesNotExist:
                error_cause = 'site does not exist'
        else:
            error_cause = 'bad site form'
            for field in form:
                for error in field.errors:
                    error_cause += ', ' + field.label + ': ' + str(error)
            for error in form.non_field_errors():
                error_cause += ', ' + str(error)
    elif request.GET['record_type'] == 'tag':
        form = EditRequestTagForm(data=request.GET)
        context['form'] = form
        if form.is_valid():
            try:
                tag = Tag.objects.get(id=form.cleaned_data['target_id'])
                form_ok = True
                context['target'] = tag.name
                if form.cleaned_data['description'] != tag.description:
                    suggestion_dict['description'] = form.cleaned_data['description']
            except Tag.DoesNotExist:
                error_cause = 'tag does not exist'
        else:
            error_cause = 'bad tag form'
            for field in form:
                for error in field.errors:
                    error_cause += ', ' + field.label + ': ' + str(error)
            for error in form.non_field_errors():
                error_cause += ', ' + str(error)
    else:
        warning('Unknown suggestion record type {}'.format(request.GET['record_type']))
        error_cause = 'unknown record type'
    # Use the assembled information to possibly create the suggestion.
    if form_ok:
        # When a change was actually requested:
        if suggestion_dict:
            if form.cleaned_data['comment']:
                EditSuggestion.objects.create(creator=request.user, suggestion=suggestion_dict,
                        record_type=form.cleaned_data['record_type'],
                        target=form.cleaned_data['target_id'],
                        target_name=context['target'],
                        comment=form.cleaned_data['comment'])
            else:
                EditSuggestion.objects.create(creator=request.user, suggestion=suggestion_dict,
                        record_type=form.cleaned_data['record_type'],
                        target=form.cleaned_data['target_id'],
                        target_name=context['target'])
            messages.add_message(request, messages.SUCCESS,
                    'We\'ve received your suggestion. Thank you!')
        else:
            messages.add_message(request, messages.WARNING,
                    'To suggest a modification, change the values on the site page and click the\
                            "Suggest yourrget changes" button.')
    else:
        messages.add_message(request, messages.ERROR,
                'We were unable to process you suggestion, sorry! ({})'.format(error_cause))
    return render(request, 'manager/suggest.html', context=context)

@login_required
def suggestionlist(request):
    suggestions = EditSuggestion.objects.filter(
            creator=request.user).order_by('-date_submitted').all()
    return render(request, 'manager/suggestionlist.html',
            { 'suggestions': suggestions })

# (staff_member_required set in urls.py)
class SuggestionDecisions(ListView):
    model = EditSuggestion
    context_object_name = 'suggestions'
    paginate_by = 25

    template_name = 'manager/suggestion_decisions.html'

    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['statuses'] = EDIT_SUGGESTION_STATUSES
        context['relevant'] = relevance_table_from_suggestions(context['suggestions'])
        return context

@staff_member_required
def decide_mod_comment(request):
    suggestion = EditSuggestion.objects.get(id=request.GET['suggestion_id'])
    suggestion.mod_comment = request.GET['new_comment']
    if not suggestion.mod_activity:
        suggestion.mod_activity = { 'mod_comment': request.user.id }
    else:
        suggestion.mod_activity['mod_comment'] = request.user.id
    suggestion.save()
    return HttpResponse('ok')

@staff_member_required
def decide_status(request):
    suggestion = EditSuggestion.objects.get(id=request.GET['suggestion_id'])
    suggestion.status = request.GET['new_status']
    if not suggestion.mod_activity:
        suggestion.mod_activity = { 'status': request.user.id }
    else:
        suggestion.mod_activity['status'] = request.user.id
    suggestion.save()
    return HttpResponse('ok')

@staff_member_required
def decide_change(request):
    suggestion = EditSuggestion.objects.get(id=request.GET['suggestion_id'])
    if suggestion.record_type == 'tag':
        tag = Tag.objects.get(id=suggestion.target)
        setattr(tag, request.GET['field'], suggestion.suggestion[request.GET['field']])
        tag.save()
    elif suggestion.record_type == 'site':
        site = Site.objects.get(id=suggestion.target)
        if request.GET['field'] != 'tags':
            setattr(site, request.GET['field'], suggestion.suggestion[request.GET['field']])
            site.save()
        else:
            desired_tags = set(suggestion.suggestion['tags'].split(' '))
            existing_tags = set(link.tag.name for link in site.tag_links.all())
            for desired_tag in desired_tags:
                if not desired_tag in existing_tags:
                    TagSiteLink.objects.create(site=site, tag=Tag.objects.get(name=desired_tag),
                            level=trust_level_to_numeric('respected'))
            for existing_tag in existing_tags:
                if not existing_tag in desired_tags:
                    TagSiteLink.objects.filter(name=existing_tag).delete()
    if not suggestion.mod_activity:
        suggestion.mod_activity = { 'change': request.user.id }
    else:
        suggestion.mod_activity['change'] = request.user.id
    suggestion.save()
    return HttpResponse('ok')

@login_required
def makerule(request):
    if request.POST:
        form = ResultRuleForm(request.POST)
    elif 'custom_rules_code' in request.GET:
        form = ResultRuleForm({ 'rule_string': request.GET['custom_rules_code'] })
    context = { 'form': form }
    if form.is_valid():
        form.instance.slug = re.sub('\\s', '-', form.cleaned_data['name'].lower())
        form.instance.assigned_user_id = request.user.id
        form.save()
        messages.add_message(request, messages.SUCCESS, 'The rule has been added.')
    return render(request, 'manager/makerule.html', context)

@login_required
def delrule(request):
    info(request.POST)
    if not request.POST or not 'delete_rule_id' in request.POST:
        return redirect('rules')
    ResultRule.objects.filter(id=int(request.POST['delete_rule_id'])).delete()
    messages.add_message(request, messages.INFO, 'The rule has been deleted.')
    return redirect('rules')

class ResultRuleList(ListView):
    model = ResultRule
    context_object_name = 'rules'
    paginate_by = 30

    template_name = 'manager/rules.html'

    def get_queryset(self):
        return ResultRule.objects.filter(assigned_user_id=self.request.user.id)

@staff_member_required
def loadblocklist(request):
    """
    Download and load URL lists intended for PiHole and other DNS blockers.
    """
    if request.method == 'POST':
        form = BlocklistForm(request.POST)
        if form.is_valid():
            list_data = urllib.request.urlopen(form.cleaned_data['blocklist_url']).read()
            for line in list_data.decode('utf-8').split('\n'):
                if not line or line[0] == '#':
                    continue
                else:
                    data_list = line.split()
                    if len(data_list) == 1: # plain lists of urls
                        if not BlockedSite.objects.filter(address=data_list[0]).exists():
                            BlockedSite.objects.create(address=data_list[0],
                                    kind=form.cleaned_data['kind_of_material'])
                    else: # 0.0.0.0 + the url
                        if not BlockedSite.objects.filter(address=data_list[1]).exists():
                            BlockedSite.objects.create(address=data_list[1],
                                    kind=form.cleaned_data['kind_of_material'])
            messages.add_message(request, messages.SUCCESS, 'The list has been loaded.')
    else:
        form = BlocklistForm()
    return render(request, 'manager/loadblocklist.html', { 'form': form })
