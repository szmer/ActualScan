from datetime import datetime, timezone
import http.client
import json
from logging import info
import re
import urllib

#from django.contrib.admin.views.decorators import staff_member_required
from django.conf import settings
from django.contrib import messages
from django.contrib.auth import login
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.contrib.auth.tokens import PasswordResetTokenGenerator
from django.contrib.sites.shortcuts import get_current_site
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render, redirect
from django.template.loader import render_to_string
from django.utils.encoding import force_bytes, force_text
from django.utils.http import urlsafe_base64_decode, urlsafe_base64_encode
from django.views.generic import ListView
import six

from bg.forms import JoinForm
from bg.models import QueryRecord, UserProfile
from bg.stop_words import STOP_WORDS
from scan.get_results import rules_results

class AccountConfirmationTokenGenerator(PasswordResetTokenGenerator):
    def _make_hash_value(self, user, timestamp):
        """
        Encode some constant user information to create the one-time token. The timestamp is
        taken from now and is stored in the token itself also.
        """
        return (six.text_type(user.pk) + six.text_type(timestamp) + six.text_type(user.password)
                + six.text_type(user.email) + six.text_type(user.userprofile.last_token_time))

account_confirmation_tokens = AccountConfirmationTokenGenerator()

def join(request):
    if request.method == 'POST':
        form = JoinForm(request.POST)
        if form.is_valid():
            user = form.save(commit=False)
            user.is_active = False
            user.save()
            user.userprofile.last_token_time = datetime.now(timezone.utc)
            user.userprofile.save()
            current_site = get_current_site(request)
            subject = 'Confirm your account email: {}'.format(current_site.domain)
            message = render_to_string('email/account_confirmation.html', {
                'user': user,
                'domain': current_site.domain,
                'uid': urlsafe_base64_encode(force_bytes(user.pk)),
                'token': account_confirmation_tokens.make_token(user),
                })
            user.email_user(subject, message)
            messages.add_message(request, messages.INFO,
                    'Thank you for joining! Please check your email to activate your account.')
            return redirect('index')
    else:
        form = JoinForm()
    return render(request, 'bg/join.html', { 'form': form })

def activation_resend(request):
    if request.method == 'POST' and 'resend_uid' in request.POST:
        user = User.objects.get(pk=int(request.POST['resend_uid']))
        if user.is_active:
            return redirect('index')
        user.userprofile.last_token_time = datetime.now(timezone.utc)
        user.userprofile.save()
        current_site = get_current_site(request)
        subject = 'Confirm your account email: {}'.format(current_site.domain)
        message = render_to_string('email/account_confirmation.html', {
            'user': user,
            'domain': current_site.domain,
            'uid': urlsafe_base64_encode(force_bytes(user.pk)),
            'token': account_confirmation_tokens.make_token(user),
            })
        user.email_user(subject, message)
        messages.add_message(request, messages.INFO,
                'We sent a new activation token to your address.')
    return redirect('index')

def activate(request, uidb64, token):
    context = dict()
    try:
        uid = force_text(urlsafe_base64_decode(uidb64))
        context['uid'] = uid
        user = User.objects.get(pk=uid)
        if user.is_active:
            return redirect('index')
        if account_confirmation_tokens.check_token(user, token):
            user.is_active = True
            user.save()
            login(request, user)
        else:
            raise ValueError('bad token')
        messages.add_message(request, messages.SUCCESS, 'Your account is now activated. Thank you!')
        return redirect('index')
    except Exception as e:
        info('Account activation failure {}'.format(e))
    return render(request, 'bg/activation_failed.html', context)

@login_required
def userprofile(request):
    if not hasattr(request.user, 'userprofile'):
        UserProfile.objects.create(person=request.user)
    if 'clear_history' in request.POST:
        messages.add_message(request, messages.INFO, 'Your search history is now empty.')
        QueryRecord.objects.filter(person=request.user).delete()
    if 'toggle_history' in request.POST:
        messages.add_message(request, messages.INFO, 'Your history preferences have been changed.')
        request.user.userprofile.wants_query_records = (not request.user.userprofile.wants_query_records)
    return render(request, 'bg/userprofile.html')

@login_required
def deleteRecord(request):
    record = QueryRecord.objects.get(id=request.GET['record_id'])
    if record.person != request.user:
        return HttpResponse('not ok')
    else:
        record.delete()
        return HttpResponse('ok')

def autosuggest(request):
    if 'q' in request.GET:
        words = request.GET['q'].split(' ')
        if len(words) == 0:
            return JsonResponse([], safe=False)
        if len(words) == 1:
            w = words[0]
            query_str = ('/solr/{}/terms?'.format(settings.SOLR_CORE)
                    +'&'.join(
                        ['terms.fl={}&terms.prefix={}'.format(field_name, urllib.parse.quote(w))
                            for field_name in settings.SOLR_TEXT_FIELDS]))
            term_conn = http.client.HTTPConnection('solr', port=8983)
            term_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
            term_response = term_conn.getresponse()
            term_response_text = term_response.read().decode('utf-8')
            term_response_json = json.loads(term_response_text)
            proposed_words = []
            is_odd = True
            for field_name in settings.SOLR_TEXT_FIELDS:
                for value in term_response_json['terms'][field_name]:
                    if is_odd:
                        proposed_words.append(value)
                        is_odd = False
                    else: # skip the frequencies
                        is_odd = True
            return JsonResponse(proposed_words, safe=False)
        else:
            result_context = rules_results(request.GET['q'],
                    settings.DEFAULT_RESULT_RULE['rule_string'])
            phrases = set()
            words = [w for w in words if w] # remove empty strings
            for doc in result_context['result']:
                for field_name in settings.SOLR_TEXT_FIELDS:
                    if field_name in doc and doc[field_name]:
                        text = doc[field_name]
                        try:
                            match_idx = re.search(
                                    '({})[\\s\\b]'.format('|'.join(
                                        # remove stopwords if possible
                                        [w for w in words if not w in STOP_WORDS] or words)),
                                    text).span()[0]
                            # Use the starting word and some subsequent ones, possibly until an important
                            # punctuation mark.
                            phrases.add(re.search('^[^\\s?!,.;]{1,}(\\s[\\w\\d_\'"-]+){0,7}',
                                text[match_idx:]).group())
                        except AttributeError: # no word found for some reason
                            pass
                        break
            return JsonResponse(list(phrases), safe=False)

    return JsonResponse([], safe=False)

class SearchesList(ListView):
    model = QueryRecord
    context_object_name = 'records'
    paginate_by = 30
    ordering = ['-date_submitted']

    template_name = 'bg/searches_list.html'
