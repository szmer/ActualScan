import http.client
import json
import re
import urllib

#from django.contrib.admin.views.decorators import staff_member_required
from django.conf import settings
from django.contrib import messages
from django.contrib.auth.decorators import login_required
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render
from django.views.generic import ListView

from bg.models import QueryRecord, UserProfile
from bg.stop_words import STOP_WORDS
from scan.get_results import rules_results

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
