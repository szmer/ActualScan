import http.client
import json
from logging import warning
import urllib

#from django.contrib.admin.views.decorators import staff_member_required
from django.conf import settings
from django.http import JsonResponse

from bg.models import AutocompleteTerm

def autosuggest(request):
    if 'q' in request.GET:
        words = request.GET['q'].split(' ')
        if len(words) == 0:
            return JsonResponse([], safe=False)
        if len(words) == 1:
            w = words[0]
            query_str = ('/solr/{}/terms?terms.fl=text&terms.prefix='.format(settings.SOLR_CORE)
                    + urllib.parse.quote(w))
            term_conn = http.client.HTTPConnection('solr', port=8983)
            term_conn.request('GET', query_str, headers={'Content-type': 'application/json'})
            term_response = term_conn.getresponse()
            term_response_text = term_response.read().decode('utf-8')
            term_response_json = json.loads(term_response_text)
            proposed_words = []
            is_odd = True
            for value in term_response_json['terms']['text']:
                if is_odd:
                    proposed_words.append(value)
                    is_odd = False
                else: # skip the frequencies
                    is_odd = True
            return JsonResponse(proposed_words, safe=False)
        else:
            try:
                term_data = AutocompleteTerm.objects.get(term=words[0]).suggest_data
            except AutocompleteTerm.DoesNotExist:
                return JsonResponse([], safe=False)
            if not 'phrases' in term_data:
                warning('No phrases data for an existing autocompletion term {}'.format(words[0]))
                return JsonResponse([], safe=False)
            matching_options = [] # matching by a string comparison
            for phrase in term_data['phrases']:
                phrase_words = phrase.split(' ')
                if len(words) <= len(phrase_words):
                    for w_n, w in enumerate(words[:-1]): # the last word maybe unfinished
                        if w != phrase_words[w_n]:
                            break
                    else:
                        matching_options.append(phrase)
            # Prefer matching phrases, but if there is none give what we have.
            return JsonResponse(matching_options or term_data['phrases'], safe=False)

    return JsonResponse([], safe=False)
