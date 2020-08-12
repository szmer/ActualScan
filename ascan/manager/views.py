from logging import info
import re
from urllib.parse import urlparse

from django.contrib import messages
from django.contrib.auth.decorators import login_required
from django.db import Error, IntegrityError
from django.shortcuts import get_object_or_404, redirect, render
from django.views.generic import ListView
from django.views.generic.detail import DetailView

from manager.forms import SiteForm, TagForm
from scan.models import Site, Tag, TagSiteLink
from scan.templatetags.scan_extras import format_trust_level

class SiteList(ListView):
    model = Site
    context_object_name = 'sites'

class SiteDetails(DetailView):
    model = Site
    context_object_name = 'site'

class TagList(ListView):
    model = Tag
    context_object_name = 'tags'

class TagDetails(DetailView):
    model = Tag
    context_object_name = 'tag'

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
    if request.method == 'POST':
        site_form = SiteForm(request.POST)
        if site_form.is_valid():
            try:
                # Derive the site name and type.
                parsed_url = urlparse(site_form.instance.homepage_url)
                site_name = re.sub('^www\\.', '', parsed_url.netloc)
                r_position = site_form.instance.homepage_url.find('/r/')
                # For reddit, the name is /r/+the subreddit name.
                if r_position != -1 and (re.search('\\.reddit\\.com$', site_name)
                        or re.search('^reddit\\.com$', site_name)):
                    site_form.instance.site_type = 'reddit'
                    site_form.instance.site_name = site_form.instance.homepage_url[r_position:]
                else:
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
            except ValueError as e:
                info(e)
                messages.add_message(request, messages.ERROR, 'There was a problem with the form.')
            except IntegrityError as e:
                info(e)
                messages.add_message(request, messages.ERROR, 'Some site data seems duplicated.')
    else:
        site_form = SiteForm()
    context = { 'form': site_form }
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
