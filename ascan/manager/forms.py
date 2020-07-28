from django.forms import ModelForm

from scan.models import Site, Tag

class SiteForm(ModelForm):
    class Meta:
        model = Site
        fields = ['homepage_url', 'search_pointer', 'source_type', 'tags']

class TagForm(ModelForm):
    class Meta:
        model = Tag
        fields = ['name', 'description']
