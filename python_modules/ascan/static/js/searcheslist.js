function deleteRecord(recordId) {
   var deleteReq = new XMLHttpRequest()
   deleteReq.addEventListener('load', function() {
      if (deleteReq.readyState == 4) {
         if (deleteReq.status == 200 && deleteReq.responseText == 'ok') {
            document.getElementById('record-'+recordId).remove()
         }
         else {
         }
      }
   })
   deleteReq.open('GET', '/bg/delete_record/?record_id='+recordId)
   deleteReq.send()
}
