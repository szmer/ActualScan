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
               '<br>Estimated additional potential (max): ', progressObj['req_stats']['future'],
               progressObj['req_stats']['reddit_blocking'] ? 
               '<br>Waiting for Reddit API allowance...' : '' 
            )
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
