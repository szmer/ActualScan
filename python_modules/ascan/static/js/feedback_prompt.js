window.addEventListener("load", function(event) {
   // We only want them visible if JS is enabled.
   var feedbackPrompts = document.querySelectorAll("span.feedback-prompt") 
   for (prompt_n = 0; prompt_n < feedbackPrompts.length; prompt_n++) {
      var prompt = feedbackPrompts[prompt_n]
      prompt.style.display = 'inline'
   }
})

function decide_feedback(is_positive) {
   var feedbSocket = new WebSocket('ws://' + window.location.host + '/ws/tagfeedback/')
   feedbSocket.onopen = function(event) {
      feedbSocket.send(JSON.stringify({'subject': 'tag_feedback', 'is_positive': is_positive,
         site: window.FEEDBACK_SITE, tag: window.FEEDBACK_TAG}))
   }
   var feedbackPrompts = document.querySelectorAll("span.feedback-prompt") 
   for (prompt_n = 0; prompt_n < feedbackPrompts.length; prompt_n++) {
      var prompt = feedbackPrompts[prompt_n]
      prompt.innerHTML = '(Thanks for answering!)'
   }
}
