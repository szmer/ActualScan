from django import forms
from django.db.models import Count
from crispy_forms.helper import FormHelper
from crispy_forms.layout import Layout, Div, Row, Column, HTML, Field
from crispy_forms.bootstrap import InlineCheckboxes

from scan.models import Site, Tag

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
    tab = forms.CharField(required=False) # the rule tab to show
    rule = forms.CharField(required=False) # the string rule, overrides the tab in terms of rule

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.helper = FormHelper()
        self.helper.disable_csrf = True # not desirable for the scan form
        self.helper.layout = Layout(
                Div('scan_query', css_class='typeahead'),
                # NOTE We have a custom template to accomodate list.js needs with the "list" class
                # and adding an invisible span with actual option text (bootstrap moves the label
                # text to ::before, ::after)
                Row(Column(InlineCheckboxes('query_tags', css_class='list', style='max-height: 200px',
                    template='widgets/form_searchable_multiplechoicefield.html')),
                    Column(InlineCheckboxes('query_sites', css_class='list', style='max-height: 200px',
                        template='widgets/form_searchable_multiplechoicefield.html'))),
                Row(Column('minimal_level')),
                HTML('<button id="search-button" type="submit" class="btn btn-primary btn-lg">Search the index</button>'
                    '{% if can_scan %} '
                    '<button id="scan-button" name="is_scan" value="true" type="submit" class="btn btn-primary btn-lg">Scan the Web</button>'
                    '{% endif %}'))

# NOTE it's important to make both forms compatible, to transfer the one from the main page into
# the index results
class EditableScanForm(forms.Form):
    """
    The form visible with the index result, where the user can refine their query.
    """
    scan_query = forms.CharField(label='Search for')
    query_tags = forms.ModelMultipleChoiceField(Tag.objects.annotate(Count('site_links')).order_by(
        '-site_links__count').all(),
        label='Tags', to_field_name='name', required=False)
    query_sites = forms.ModelMultipleChoiceField(Site.objects.annotate(
        Count('scraperequest')).order_by('-scraperequest__count').all(),
        label='Sites', to_field_name='site_name', required=False)
    minimal_level = forms.ChoiceField(
            label='Minimal level',
            choices=[(x, x) for x in ['spam', 'community', 'respected', 'base']],
            initial='community')
    tab = forms.CharField(required=False) # the rule tab to show
    rule = forms.CharField(required=False) # the string rule, overrides the tab in terms of rule

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.helper = FormHelper()
        self.helper.disable_csrf = True # not desirable for the scan form
        self.helper.layout = Layout(
                # NOTE We have a custom template to accomodate list.js needs with the "list" class
                # and adding an invisible span with actual option text (bootstrap moves the label
                # text to ::before, ::after)
                Row(Column(Field('scan_query', css_class='typeahead form-control-lg')),
                    Column(InlineCheckboxes('query_tags', css_class='list',
                        template='widgets/form_searchable_multiplechoicefield.html',
                        style='max-height: 100px')),
                    Column(InlineCheckboxes('query_sites', css_class='list',
                        template='widgets/form_searchable_multiplechoicefield.html',
                        style='max-height: 100px'))),
                Row(Column('minimal_level')),
                HTML('<button type="submit" id="search-button" class="btn btn-primary btn-lg">Search the index</button>'
                    '{% if can_scan %} '
                    '<button name="is_scan" id="scan-button" value="true" type="submit" class="btn btn-primary btn-lg">Scan the Web</button>'
                    '{% endif %}'))
