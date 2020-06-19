export function updateScanState() {
   var current_time = new Date();
   current_time = current_time.getSeconds();
   if (current_seconds % 2 == 0) {
      const scan_id = window.scanID;
      var xhttp = new XMLHttpRequest();
      xhttp.onreadystatechange = function() {
          if (this.readyState == 4 && this.status == 200) {
             // Typical action to be performed when the document is ready:
             document.getElementById("demo").innerHTML = xhttp.responseText;
          }
      };
   }
   window.requestAnimationFrame(updateScanState);
}
