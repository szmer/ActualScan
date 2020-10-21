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
      if (typeof itemDivs[itemN].getElementsByClassName != 'function') {
         continue
      }
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

function requestNewResults() {
   window.lastRulesMessage = new Date()
   rulesUpdateSocket.send(JSON.stringify({
      'subject': 'rules_results',
      'rules': window.RULES_STRING,
      'query_phrase': window.QUERY_PHRASE,
      'sites': window.SITES,
      'tags': window.TAGS
   }))
}

function updateScoringRules(fieldName) {
   // Collect the string rules. If we're coming from a slider, it will be changed, if from the string
   // input, it will serve as the truth source for updating sliders.
   if (fieldName == 'ALL') {
      window.RULES_STRING = document.querySelector('#rules_code').value
   }
   var strRuleFields = window.RULES_STRING ? window.RULES_STRING.split(';') : []
   var rules = new Object()
   for (var ruleFieldN in strRuleFields) {
      var ruleField = strRuleFields[ruleFieldN]
      var parts = ruleField.split(',')
      rules[parts[0]] = parts.slice(1)
   }

   if (fieldName != 'ALL') {
      // Update the interface indicators.
      if (fieldName.search('date') == -1) {
         document.querySelector('#'+fieldName+'_min_val').innerHTML = document.querySelector('#'+fieldName+'_min').value
         document.querySelector('#'+fieldName+'_max_val').innerHTML = document.querySelector('#'+fieldName+'_max').value
      }
      document.querySelector('#'+fieldName+'_weight_val').innerHTML = document.querySelector('#'+fieldName+'_weight').value
      var minValue = (document.querySelector('#'+fieldName+'_min').value == document.querySelector('#'+fieldName+'_min').getAttribute('min')
         ? '*' : document.querySelector('#'+fieldName+'_min').value)
      var maxValue = (document.querySelector('#'+fieldName+'_max').value == document.querySelector('#'+fieldName+'_max').getAttribute('max')
         ? '*' : document.querySelector('#'+fieldName+'_max').value)
      rules[window.FEATURE_CODES[fieldName]] = [
         minValue, maxValue,
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
   }
   // The 'ALL' case (manual change of rules string).
   else {
      for (var fieldName in window.FEATURE_CODES) {
         var fieldCode = window.FEATURE_CODES[fieldName]
         var localRules = (rules[ruleFieldCode] || [fieldCode, '*', '*', '0'])
         document.querySelector('#'+fieldName+'_min').value = localRules[1] == '*' ? document.querySelector('#'+fieldName+'_min').getAttribute('min') : localRules[1]
         document.querySelector('#'+fieldName+'_max').value = localRules[2] == '*' ? document.querySelector('#'+fieldName+'_max').getAttribute('max') : localRules[2]
         if (fieldName.search('date') == -1) {
            document.querySelector('#'+fieldName+'_min_val').innerHTML = localRules[1] == '*' ? document.querySelector('#'+fieldName+'_min').getAttribute('min') : localRules[1]
            document.querySelector('#'+fieldName+'_max_val').innerHTML = localRules[2] == '*' ? document.querySelector('#'+fieldName+'_max').getAttribute('max') : localRules[2]
         }
         document.querySelector('#'+fieldName+'_weight').rules = localRules[3]
         document.querySelector('#'+fieldName+'_weight_val').innerHTML = localRules[3]
      }
   }

   // Issue a throttled request for new results.
   var nowTime = new Date()
   var delay = (nowTime - window.lastRulesMessage)
   if (delay <= window.rulesMessageDelay) {
      if (window.rulesRequestWaiting) {
         window.clearTimeout(window.rulesRequestWaiting)
      }
      window.rulesRequestWaiting = window.setTimeout(requestNewResults, window.rulesMessageDelay-delay)
   }
   else {
      requestNewResults()
   }
}
