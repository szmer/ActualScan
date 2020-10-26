function markChangingComment(suggestionId) {
   var commentInput = document.getElementById('comment-'+suggestionId)
   if (commentInput.className.search('bg-warning') == -1) {
      commentInput.className += ' bg-warning'
   }
}

function decideChange(suggestionId, changeField) {
   document.getElementById('server-response-'+suggestionId).innerHTML = '...'
   var changeReq = new XMLHttpRequest()
   changeReq.addEventListener('load', function() {
      if (changeReq.readyState == 4) {
         if (changeReq.status == 200 && changeReq.responseText == 'ok') {
            document.getElementById('server-response-'+suggestionId).innerHTML = 'Done!'
            document.getElementById('sugg-'+suggestionId+'-'+changeField).className = 'strikethrough'
            document.getElementById('change-'+suggestionId+'-'+changeField).remove()
         }
         else {
            document.getElementById('server-response-'+suggestionId).innerHTML = 'Failed'
         }
      }
   })
   changeReq.open('GET', '/manager/decide_change/?suggestion_id='+suggestionId+'&field='+changeField)
   changeReq.send()
}

function decideComment(suggestionId) {
   document.getElementById('server-response-'+suggestionId).innerHTML = '...'
   var modCommentReq = new XMLHttpRequest()
   modCommentReq.addEventListener('load', function() {
      if (modCommentReq.readyState == 4) {
         if (modCommentReq.status == 200 && modCommentReq.responseText == 'ok') {
            document.getElementById('server-response-'+suggestionId).innerHTML = 'Done!'
            document.getElementById('comment-'+suggestionId).className = (
               document.getElementById('comment-'+suggestionId).className.replace('bg-warning', ''))
         }
         else {
            document.getElementById('server-response-'+suggestionId).innerHTML = 'Failed'
         }
      }
   })
   var newComment = document.getElementById('comment-'+suggestionId).value
   modCommentReq.open('GET', '/manager/decide_mod_comment/?suggestion_id='+suggestionId+'&new_comment='+newComment)
   modCommentReq.send()
}

function decideStatus(suggestionId, newStatus, newStatusName) {
   document.getElementById('server-response-'+suggestionId).innerHTML = '...'
   var statusReq = new XMLHttpRequest()
   statusReq.addEventListener('load', function() {
      if (statusReq.readyState == 4) {
         if (statusReq.status == 200 && statusReq.responseText == 'ok') {
            document.getElementById('server-response-'+suggestionId).innerHTML = 'Done!'
            document.getElementById('status-badge-'+suggestionId).innerHTML = newStatusName
         }
         else {
            document.getElementById('server-response-'+suggestionId).innerHTML = 'Failed'
         }
      }
   })
   statusReq.open('GET', '/manager/decide_status/?suggestion_id='+suggestionId+'&new_status='+newStatus)
   statusReq.send()
}
