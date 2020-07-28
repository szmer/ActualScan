from django import forms

from scan.models import Tag

class PublicScanForm(forms.Form):
    scan_query = forms.CharField(label='Search for')
    query_tags = forms.ModelMultipleChoiceField(Tag.objects.all(),
            label='On sites with these tags')
