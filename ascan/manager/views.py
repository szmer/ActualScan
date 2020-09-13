from logging import info, warning
import re
from urllib.parse import urlparse

from django.core.validators import URLValidator
from django.core.exceptions import ValidationError
from django.contrib import messages
from django.contrib.auth.decorators import login_required
from django.db import Error, IntegrityError
from django.shortcuts import get_object_or_404, redirect, render
from django.views.generic import ListView
from django.views.generic.detail import DetailView

from manager.forms import SiteForm, TagForm, EditRequestSiteForm, EditRequestTagForm
from manager.models import EditSuggestion
from scan.models import Site, Tag, TagSiteLink, ScanJob, ScrapeRequest
from scan.templatetags.scan_extras import format_trust_level

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

class TagDetails(DetailView):
    model = Tag
    context_object_name = 'tag'

def site_details(request, pk):
    site = Site.objects.get(id=pk)
    form = EditRequestSiteForm(instance=site,
            initial={'tags': [link.tag for link in site.tag_links.all()]})
    context = { 'site': site, 'form': form }
    return render(request, 'scan/site_detail.html', context=context)

def tagname(request, tag_name):
    """
    Look up the tag by name instead of the primary key.
    """
    tag = get_object_or_404(Tag, name=tag_name)
    return render(request, 'scan/tag_detail.html', { 'tag': tag })

def scanlist(request):
    jobs = ScanJob.objects.filter(user=request.user).order_by('-status_changed').all()
    page_counts = []
    for job in jobs:
        page_counts.append(ScrapeRequest.objects.filter(job=job).count())
    return render(request, 'manager/scanlist.html', { 'scans': zip(jobs, page_counts) })

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
    if request.method == 'POST':
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
                        parsed_url = urlparse(site_form.instance.search_pointer)
                    except ValidationError as e:
                        raise SearchURLParsingError(site_form.instance.search_pointer, *e.args)
                    except ValueError as e:
                        raise SearchURLParsingError(site_form.instance.search_pointer, *e.args)
                    url_later_part = (parsed_url.path+parsed_url.params+parsed_url.query
                            +parsed_url.fragment)
                    if not (Site.MOCK_STR1 in url_later_part and Site.MOCK_STR2 in url_later_part):
                        info(Site.MOCK_STR1+' '+parsed_url.path+parsed_url.params+parsed_url.query)
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
                return redirect('manager:sites')
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
                        'form.')
    else:
        site_form = SiteForm()
    context = { 'form': site_form, 'site_form_submitted': submitted }
    return render(request, 'manager/makesite.html', context)

@login_required
def maketag(request):
    if request.method == 'POST':
        tag_form = TagForm(request.POST)
        if tag_form.is_valid():
            tag_form.instance.creator = request.user
            tag_form.save()
            messages.add_message(request, messages.SUCCESS, 'The tag {} has been added.'.
                    format(tag_form.instance.name))
            # Return to the list of tags.
            return redirect('manager:tags')
    else:
        tag_form = TagForm()
    context = { 'form': tag_form }
    return render(request, 'manager/maketag.html', context)

@login_required
def suggest(request):
    context = { 'record_type': request.GET['record_type'], 'target': request.GET['target'] }
    suggestion_dict = {} # we'll fill it only with the values that are changed
    form_ok = False
    # Process possible suggestion types.
    if request.GET['record_type'] == 'site':
        form = EditRequestSiteForm(data=request.GET)
        if form.is_valid():
            form_ok = True
            site = Site.objects.get(site_name=form.cleaned_data['target'])
            context['target_id'] = site.id
            for field in ['site_type', 'source_type', 'homepage_url', 'search_pointer']:
                if form.cleaned_data[field] != getattr(site, field):
                    # This field is hidden and non-suggestable for reddit sites.
                    if not (field == 'search_pointer' and site.site_type == 'reddit'):
                        suggestion_dict[field] = form.cleaned_data[field]
            if form.cleaned_data['tags'] != [link.tag for link in site.tag_links.all()]:
                suggestion_dict['tags'] = ' '.join([tag.name for tag in form.cleaned_data['tags']])
    elif request.GET['record_type'] == 'tag':
        form = EditRequestTagForm(data=request.GET)
        if form.is_valid():
            form_ok = True
            tag = Tag.objects.get(name=form.cleaned_data['target'])
            context['target_id'] = tag.id
            if form.cleaned_data['description'] != tag.description:
                suggestion_dict['description'] = form.cleaned_data['description']
    else:
        warning('Unknown suggestion record type {}'.format(request.GET['record_type']))
    # Use the assembled information to possibly create the suggestion.
    if form_ok:
        # When a change was actually requested:
        if suggestion_dict:
            suggestion_dict['target'] = form.cleaned_data['target']
            suggestion_dict['record_type'] = form.cleaned_data['record_type']
            EditSuggestion.objects.create(creator=request.user, suggestion=suggestion_dict)
            messages.add_message(request, messages.SUCCESS,
                    'We\'ve received your suggestion. Thank you!')
        else:
            messages.add_message(request, messages.WARNING,
                    'To suggest a modification, change the values on the site page and click the\
                            "Suggest yourrget changes" button.')
    else:
        messages.add_message(request, messages.ERROR,
                'We were unable to process you suggestion, sorry! Please contact the admins.')
    return render(request, 'manager/suggest.html', context=context)

@login_required
def suggestionlist(request):
    suggestions = EditSuggestion.objects.filter(
            creator=request.user).order_by('-date_submitted').all()
    obj_ids = []
    for sug in suggestions:
        if not 'target' in sug.suggestion or not 'record_type' in sug.suggestion:
            obj_ids.append(-1)
            continue
        if sug.suggestion['record_type'] == 'site':
            obj_ids.append(Site.objects.get(site_name=sug.suggestion['target']).id)
        elif sug.suggestion['record_type'] == 'tag':
            obj_ids.append(Tag.objects.get(name=sug.suggestion['target']).id)
        else:
            obj_ids.append(-1)
    return render(request, 'manager/suggestionlist.html',
            { 'suggestions': zip(suggestions, obj_ids) })
