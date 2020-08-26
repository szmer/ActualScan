window.addEventListener("load", function(event) {
   if (window.location.search.search('query_phrase') != -1) {
      var goToOptions = document.getElementById('go_to_results_options')

      var goToSwitchLabel = document.createElement('label')
      goToSwitchLabel.innerHTML = 'Go to the results when finished'
      goToSwitchLabel.setAttribute('for', 'go_to_results_switch')
      goToSwitch = document.createElement('input')
      goToSwitch.setAttribute('type', 'checkbox')
      goToSwitch.setAttribute('name', 'go_to_results_switch')
      goToSwitch.id = 'go_to_results_switch'
      goToSwitch.checked = true
      goToSwitchLabel.appendChild(goToSwitch)
      goToOptions.appendChild(goToSwitchLabel)

      var goToButton = document.createElement('button')
      goToButton.className = 'btn btn-secondary'
      goToButton.innerHTML = 'Get index results now'
      goToButton.onclick = function(event) {
         window.location.href = window.location.href.replace('is_scan=true', '')
      }
      goToOptions.appendChild(goToButton)
   }
})

scanSocket = new WebSocket('ws://' + window.location.host + '/ws/scanprogr/')
scanSocket.onopen = function(event) {
   scanSocket.send(JSON.stringify({'subject': 'inquire_scan', 'scan_id': window.SCAN_ID}))
}
scanSocket.onmessage = function(event) {
   if(event['data'] == 'inquiry_refused') {
      console.log(event['data'])
      return
   }
   else if(event['data'] != 'inquiry_accepted') {
      try {
         var eventObj = JSON.parse(event['data'])
         if(eventObj['type'] == 'progress.info') {
            var progressObj = JSON.parse(eventObj['text'])['content']
            document.getElementById('scan_phase').innerHTML = progressObj['phase']
            if(progressObj['phase'] == 'working') {
               document.getElementById('scan_fails').innerHTML = progressObj['fails']
               document.getElementById('scan_last_url').innerHTML = progressObj['last_url']
               document.getElementById('scan_dl_proportion').innerHTML = (parseFloat(
                  progressObj['dl_proportion']) * 100).toFixed(2) + '%'
               document.getElementById('scan_req_stats').innerHTML = ''.concat(
                  'Search pages: ' ,  progressObj['req_stats']['searches'],
                  ', content pages: ', progressObj['req_stats']['crawls'],
                  '<br>Committed: ', progressObj['req_stats']['commits'],
                  ', waiting and being processed: ', progressObj['req_stats']['wait_runs'],
                  ', failed: ', progressObj['req_stats']['fails'],
                  '<br>Estimated potential (max): ', progressObj['req_stats']['future'],
                  progressObj['req_stats']['reddit_blocking'] ? 
                  '<br>Waiting for Reddit API allowance...' : '' 
               )
            }
            if(progressObj['phase'] == 'finished'
               && document.getElementById('go_to_results_switch').checked) {
               window.location.href = window.location.href.replace('is_scan=true', '')
            }
         }
         else {
            console.log('Unknown websocket event subject in '+JSON.stringify(eventObj))
         }
      }
      catch(error) {
         console.error('Error '+error+' with '+JSON.stringify(event))
      }
   }
}
