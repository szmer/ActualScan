window.addEventListener("load", function(event) {
   var spinnerCode = '<span class="spinner-border spinner-border-sm" role="status" aria-hidden="true"></span> '
   document.getElementById('search-button').addEventListener("click", function(event) {
      document.getElementById('search-button').innerHTML = spinnerCode + document.getElementById('search-button').innerHTML
   })
   document.getElementById('scan-button').addEventListener("click", function(event) {
      document.getElementById('scan-button').innerHTML = spinnerCode + document.getElementById('scan-button').innerHTML
   })
})
