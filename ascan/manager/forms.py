from django import forms
from django.forms import ModelForm

from scan.models import Site, Tag

class SiteForm(ModelForm):
    tags = forms.ModelMultipleChoiceField(Tag.objects.all(),
            label='Site tags')
    class Meta:
        model = Site
        fields = ['homepage_url', 'search_pointer', 'source_type']

class TagForm(ModelForm):
    class Meta:
        model = Tag
        fields = ['name', 'description']
