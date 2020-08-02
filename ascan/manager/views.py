import re
from urllib.parse import urlparse

from django.contrib import messages
from django.contrib.auth.decorators import login_required
from django.shortcuts import get_object_or_404, redirect, render
from django.views.generic import ListView
from django.views.generic.detail import DetailView

from manager.forms import SiteForm, TagForm
from scan.models import Site, Tag

class SiteList(ListView):
    model = Site
    context_object_name = 'sites'

class SiteDetails(DetailView):
    model = Site
    context_object_name = 'site'

class TagSiteList(ListView):
    template_name = 'scan/site_list.html'
    context_object_name = 'sites'
    def get_queryset(self):
        self.tag = get_object_or_404(Tag, name=self.kwargs['tag'])
        return self.tag.sites.all()
    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['tag'] = self.kwargs['tag']
        context['tag_id'] = get_object_or_404(Tag, name=self.kwargs['tag']).id
        return context

class TagList(ListView):
    model = Tag
    context_object_name = 'tags'

class TagDetails(DetailView):
    model = Tag
    context_object_name = 'tag'

@login_required
def makesite(request):
    if request.method == 'POST':
        site_form = SiteForm(request.POST)
        if site_form.is_valid():
            try:
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
                site_form.instance.creator = request.user
                site_form.save()
                messages.add_message(request, messages.SUCCESS, 'The site {} has been added.'.
                        format(site_form.instance.site_name))
                # Return to the list of sites.
                return redirect('manager:sites')
            except ValueError:
                messages.add_message(request, messages.ERROR, 'There had a problem with the form.')
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
