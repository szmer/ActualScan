window.addEventListener("load", function(event) {
   if (! window.SITE_FORM_SUBMITTED) {
      document.getElementById('div_id_homepage_url').style.display = 'none'
   }

   document.getElementById('id_search_pointer').addEventListener("input", function(event) {
      var searchUrl = document.getElementById('id_search_pointer').value
      if (searchUrl.trim().length > 0) {
         document.getElementById('div_id_homepage_url').style.display = 'block'
         var redditRegex = /^(http)?s?(:\/\/)?[^\/]*(reddit\.com)?\/r\//
         var match = searchUrl.match(redditRegex)
         if (match) {
            var endOfAddr = searchUrl.substring(match[0].length).search('/')
            var homeUrl = searchUrl
            if (endOfAddr != -1) {
               homeUrl = homeUrl.substring(0, match[0].length+endOfAddr)
            }
            document.getElementById('id_homepage_url').value = homeUrl
            document.getElementById("id_source_type_1").checked = false
            document.getElementById("id_source_type_2").checked = true
            document.getElementById("id_source_type_3").checked = false
            document.getElementById("id_source_type_4").checked = false
         }
         else {
            var url = new URL(searchUrl)
            document.getElementById('id_homepage_url').value = url.hostname
         }
      }
   })
})
