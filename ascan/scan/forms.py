from datetime import datetime, timedelta, timezone

from django import forms
from django.db.models import Count
from crispy_forms.helper import FormHelper
from crispy_forms.layout import Layout, Div, Row, Column, HTML
from crispy_forms.bootstrap import InlineCheckboxes
from dynamic_preferences.registries import global_preferences_registry

from scan.models import Site, Tag
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
    query_tags = forms.ModelMultipleChoiceField(Tag.objects.annotate(Count('site_links')).order_by(
        '-site_links__count').all(),
        label='If you want, limit to these tags...', to_field_name='name', required=False)
    query_sites = forms.ModelMultipleChoiceField(Site.objects.annotate(
        Count('scraperequest')).order_by('-scraperequest__count').all(),
        label='...and/or these sites', to_field_name='site_name', required=False)
    minimal_level = forms.ChoiceField(
            label='Minimal trustworthiness level',
            choices=[(x, x) for x in ['spam', 'community', 'respected', 'base']],
            initial='community')
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

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.helper = FormHelper()
        self.helper.disable_csrf = True # not desirable for the scan form
        self.helper.layout = Layout(
                Div('scan_query', css_class='typeahead'),
                # NOTE We have a custom template to accomodate list.js needs with the "list" class
                # and adding an invisible span with actual option text (bootstrap moves the label
                # text to ::before, ::after)
                Row(Column(InlineCheckboxes('query_tags', css_class='list',
                    template='widgets/form_searchable_multiplechoicefield.html')),
                    Column(InlineCheckboxes('query_sites', css_class='list',
                        template='widgets/form_searchable_multiplechoicefield.html'))),
                Row(Column('start_date'),
                    Column('end_date')),
                Row(Column('allow_undated'),
                    Column('minimal_level')),
                HTML('<button type="submit" class="btn btn-primary btn-lg">Search the index</button>'
                    '{% if can_scan %} '
                    '<button name="is_scan" value="true" type="submit" class="btn btn-primary btn-lg">Scan the Web</button>'
                    '{% endif %}'))
