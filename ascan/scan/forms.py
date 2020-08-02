from datetime import datetime, timedelta, timezone

from django import forms
from dynamic_preferences.registries import global_preferences_registry

from scan.models import Tag
from scan.widgets import MonthPickerInput

def get_default_timedelta():
    global_preferences = global_preferences_registry.manager()
    return (datetime.now(timezone.utc) -  timedelta(
            weeks=global_preferences['default_scan_timedelta_start'])
            ).strftime('%m/%Y')

def get_next_month():
    return (datetime.now(timezone.utc) +  timedelta(weeks=5)).strftime('%m/%Y')

class PublicScanForm(forms.Form):
    scan_query = forms.CharField(label='Search for')
    query_tags = forms.ModelMultipleChoiceField(Tag.objects.all(),
            label='On sites with these tags')
    minimal_level = forms.ChoiceField(
            label='Minimal trustworthiness level',
            choices=[(x, x) for x in ['spam', 'community', 'respected', 'base']],
            initial='community'
            )
    start_date = forms.DateTimeField(
            label='Go through sites from this time onwards',
            input_formats=['%m/%Y'],
            widget=MonthPickerInput,
            initial=get_default_timedelta
            )
    end_date = forms.DateTimeField(
            label='Go through sites ending on that date',
            input_formats=['%m/%Y'],
            widget=MonthPickerInput,
            initial=get_next_month
            )
    allow_undated = forms.BooleanField(initial=True, required=False) # don't require to allow False
