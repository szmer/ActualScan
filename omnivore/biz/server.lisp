(in-package :omnivore)

;;;
;;;
(defun sent-template-representation (sent)
  (list :sent-text (raw-text sent)
        :date (if (read-attribute sent "publication-date")
                ;; KLUDGE poor man's formatting
                (subseq (read-attribute sent "publication-date") 0 10)
                "")
        :author (format nil "~A on ~A"
                        (or (read-attribute sent "creator") "someone")
                        (read-attribute sent "site_name"))
        :url (read-attribute sent "url")
        :domain (when (read-attribute sent "url")
                  (purl:url-host (purl:url (read-attribute sent "url"))))))

;;; Clear what may remain from the earlier server objects
(when (and (boundp '*server*) *server*) (hunchentoot:stop *server* :soft nil))
(defparameter *server* nil)

;;;
;;; Static stuff.
(push (hunchentoot:create-static-file-dispatcher-and-handler
        "/" (merge-pathnames (pathname "index.html") *html-path*))
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-static-file-dispatcher-and-handler
        "/index.html" (merge-pathnames (pathname "index.html") *html-path*))
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-static-file-dispatcher-and-handler
        "/css/lkp.css" (merge-pathnames (pathname "css/lkp.css") *html-path*))
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-static-file-dispatcher-and-handler
        "/css/chartist.min.css" (merge-pathnames (pathname "css/chartist.min.css") *html-path*))
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-static-file-dispatcher-and-handler
        "/js/chartist.min.js" (merge-pathnames (pathname "js/chartist.min.js") *html-path*))
      hunchentoot:*dispatch-table*)

;;;
;;; Dynamic responders.
(hunchentoot:define-easy-handler (query-response :uri "/result") (q)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((query-result (query-result-solr! q))
        (query-response-template
          (html-template:create-template-printer
            (merge-pathnames (pathname "result.tmpl") *html-path*)
            :force t)))
    (with-output-to-string (string-stream)
      (html-template:fill-and-print-template
        query-response-template
        (list :query-text q
              :typical
              (mapcar #'sent-template-representation
                      (getf query-result :typical))
              :atypical
              (mapcar #'sent-template-representation
                      (getf query-result :atypical))
              :phrases
              (mapcar (lambda (phrase-entry)
                        (list :phrase (cl-strings:join (getf phrase-entry :phrase) :separator " ")
                              :typical
                              (mapcar #'sent-template-representation
                                      (getf phrase-entry :typical))))
                      (getf query-result :phrases))
              :sites-stats
              (mapcar (lambda (site-entry) (list :site (car site-entry)
                                                 :frequency (cdr site-entry)))
                      (getf query-result :sites-stats)))
        :stream string-stream))))

;;;
;;; Start the server.
(setf *server* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start *server*)
