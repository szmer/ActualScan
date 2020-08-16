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
            if(progressObj['phase'] == 'crawl') {
               document.getElementById('scan_fails').innerHTML = progressObj['fails']
               document.getElementById('scan_last_url').innerHTML = progressObj['last_url']
               document.getElementById('scan_dl_proportion').innerHTML = (parseFloat(
                  progressObj['dl_proportion']) * 100).toFixed(2) + '%'
            }
            if(progressObj['phase'] == 'finished') {
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
