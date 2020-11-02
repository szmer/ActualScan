import re

from django import forms
from django.db.models import Count
from django.forms import Form, ModelForm, CharField, URLField, ValidationError
from django.utils.translation import gettext_lazy as _
from crispy_forms.helper import FormHelper
from crispy_forms.layout import Layout, Div, HTML, Submit
from crispy_forms.bootstrap import InlineCheckboxes, InlineRadios
from better_profanity import profanity

from scan.models import Site, Tag, ResultRule
from .models import BlockedSite

profanity.load_censor_words(
        # Allow some cussing that may appear in common language
        whitelist_words=['bitch', 'bullshit', 'crap', 'damn', 'dick', 'dildo', 'shit'])

class CleanCharField(CharField):
    def validate(self, value):
        super().validate(value)
        censored = profanity.censor(value, '✨')
        if '✨' in censored:
            raise ValidationError(
                    'Sorry! Our abuse filter did not like some of the language you\'ve used. '
                    'Please change it or contact the admins to add the content.')

class SimpleCrawlForm(forms.Form):
    """
    The form to start a simple crawl of a site.
    """
    crawl_site = forms.ModelChoiceField(Site.objects.annotate(
        Count('scraperequest')).order_by('-scraperequest__count').all())

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Remove the empty choice.
        self.fields['crawl_site'].choices = list(self.fields['crawl_site'].choices)[1:]

        self.helper = FormHelper()
        self.helper.form_action = '/scan/crawl/'
        self.helper.layout = Layout(
                InlineRadios('crawl_site', css_class='list',
                        template='widgets/form_searchable_singlechoicefield.html',
                        style='max-height: 100px'),
                HTML('<button type="submit" id="search-button" class="btn btn-primary">Crawl the site</button>'))

class SiteForm(ModelForm):
    """
    The form for adding sites to the directory.
    """
    tags = forms.ModelMultipleChoiceField(Tag.objects.all(), label='Site tags')

    class Meta:
        model = Site
        fields = ['search_pointer', 'homepage_url', 'source_type']
        labels = {
                'source_type': _('Type of content'),
                'homepage_url': _('Address of the homepage of the site (like <em>en.wikipedia.org</em>).'),
                'search_pointer': _('Hi! Please find the <strong>search</strong> 🔍 function in the'
                    ' site you want to add, and search for the following:  '
                    '<span style="display: inline" class="card p-2 text-muted">twenty cats</span>'
                    ' (copy contents of the box, including the vertical pipes). Then paste the '
                    '<strong>address</strong> of the search page that you get below.<br>'
                    '<img src="/static/scan/search_pointer.png" style="max-width: 80%"><br>'
                    '(Psst! If you want add a <strong>subreddit</strong>, you can just paste its '
                    'address or the <em>/r/name</em>, I will find its search when you\'ll add the '
                    'site!)')
                }

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Remove the empty choice.
        self.fields['source_type'].choices = self.fields['source_type'].choices[1:]

        self.helper = FormHelper()
        self.helper.layout = Layout(
                Div('search_pointer'),
                Div('homepage_url'),
                Div(InlineRadios('source_type')),
                Div(InlineCheckboxes('tags', css_class='list',
                    template='widgets/form_searchable_multiplechoicefield.html')),
                HTML('<button type="submit" class="btn btn-primary">Add the site</button>')
                )

class TagForm(ModelForm):
    """
    The form for adding tags to the directory.
    """
    class Meta:
        model = Tag
        fields = ['name', 'description']
        # Apply the profanity filtering.
        field_classes = {
                'name': CleanCharField,
                'description': CleanCharField
                }

class EditRequestSiteForm(ModelForm):
    """
    This form is rendered 'manually' in the site details template, this class is used for validation,
    """
    # Make this optional to hide the field for Reddit sites.
    search_pointer = forms.CharField(max_length=Site._meta.get_field('search_pointer').max_length,
            required=False)
    tags = forms.ModelMultipleChoiceField(Tag.objects.all(), label='')
    record_type = forms.CharField()
    target_id = forms.IntegerField()
    comment = forms.CharField(required=False)

    class Meta:
        model = Site
        fields = ['site_type', 'source_type', 'homepage_url']

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.helper = FormHelper()
        self.helper.layout = Layout(
                Div(InlineCheckboxes('tags', css_class='list',
                    template='widgets/form_searchable_multiplechoicefield.html'))
                )

class EditRequestTagForm(ModelForm):
    """
    This form is rendered 'manually' in the site details template, this class is used for validation,
    """
    record_type = forms.CharField()
    target_id = forms.IntegerField()
    comment = forms.CharField(required=False)

    class Meta:
        model = Tag
        fields = ['description']

class RuleStringField(CharField):
    def validate(self, value):
        super().validate(value)
        if not re.match('^(\\w+,[\\d.*]+,[\\d.*]+,[\\d.*]+;)*\\w+,[\\d.*]+,[\\d.*]+,[\\d.*]+$',
                value):
            raise ValidationError('We could not parse the rule into elements.')
        rule_entries = value.split(';')
        for entry in rule_entries:
            fields = entry.split(',')
            for field in fields[1:]:
                if not field == '*':
                    try:
                        float(field)
                    except ValueError:
                        raise ValidationError('Could not parse the number {}.'.format(field))

class ResultRuleForm(ModelForm):
    class Meta:
        model = ResultRule
        fields = ['name', 'rule_string']
        field_classes = {
                'rule_string': RuleStringField,
                }
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.helper = FormHelper()
        self.helper.add_input(Submit('submit', 'Submit', css_class='btn-primary'))

class BlocklistForm(Form):
    blocklist_url = URLField()
    kind_of_material = CharField(max_length=BlockedSite._meta.get_field('kind').max_length)
