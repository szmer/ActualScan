rulesUpdateSocket = new WebSocket('ws://' + window.location.host + '/ws/rulesupdate/')

rulesUpdateSocket.onmessage = function(event) {
   var resultsDiv = document.querySelector('#main-results')
   var data = JSON.parse(event['data'])
   if (data.length == 0) {
      document.getElementById('results-status').innerHTML = 'Nothing found.'
   }
   else {
      document.getElementById('results-status').innerHTML = ''
   }
   var itemDivs = resultsDiv.getElementsByClassName('list-group-item')
   for (var itemN in itemDivs) {
      itemDivs[itemN].getElementsByClassName('result-url')[0].innerHTML = data[itemN]['url']
      itemDivs[itemN].getElementsByClassName('result-url')[0].href = data[itemN]['url']
      itemDivs[itemN].getElementsByClassName('result-text')[0].innerHTML = (
         (data[itemN]['text_en']||'')+(data[itemN]['text_xx']||'')
      )
      itemDivs[itemN].getElementsByClassName('result-author')[0].innerHTML = data[itemN]['author']
      itemDivs[itemN].getElementsByClassName('result-date')[0].innerHTML = data[itemN]['date_post']
      itemDivs[itemN].getElementsByClassName('result-site')[0].innerHTML = data[itemN]['site_name']
   }
}

function updateScoringRules(fieldName) {
   // Update the interface indicators.
   if (fieldName.search('date') == -1) {
      document.querySelector('#'+fieldName+'_min_val').innerHTML = document.querySelector('#'+fieldName+'_min').value
      document.querySelector('#'+fieldName+'_max_val').innerHTML = document.querySelector('#'+fieldName+'_max').value
   }
   document.querySelector('#'+fieldName+'_weight_val').innerHTML = document.querySelector('#'+fieldName+'_weight').value
   // Collect new rules.
   var oldRuleFields = window.RULES_STRING.split(';')
   var rules = new Object()
   for (var ruleFieldN in oldRuleFields) {
      var ruleField = oldRuleFields[ruleFieldN]
      var parts = ruleField.split(',')
      rules[parts[0]] = parts.slice(1)
   }
   rules[window.FEATURE_CODES[fieldName]] = [
      document.querySelector('#'+fieldName+'_min').value, 
      document.querySelector('#'+fieldName+'_max').value,
      document.querySelector('#'+fieldName+'_weight').value
   ]
   // Convert rules to the code string.
   var rulesStr = ''
   for (var ruleFieldCode in rules) {
      rulesStr += ruleFieldCode + ',' + rules[ruleFieldCode].join(',') + ';'
   }
   rulesStr = rulesStr.slice(0, rulesStr.length-1) // strip the final semicolon
   window.RULES_STRING = rulesStr
   document.querySelector('#rules_code').value = window.RULES_STRING
   var nowTime = new Date()
   if ((nowTime - window.lastRulesMessage) > window.rulesMessageDelay) {
      rulesUpdateSocket.send(JSON.stringify({
         'subject': 'rules_results',
         'rules': window.RULES_STRING,
         'query_phrase': window.QUERY_PHRASE,
         'sites': window.SITES,
         'tags': window.TAGS
      }))
      window.lastRulesMessage = nowTime
   }
}
