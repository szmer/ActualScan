(in-package :omnivore2)

;;; Clear what may remain from the earlier server objects
(when (boundp '*server*)
  (ignore-errors (hunchentoot:stop *server* :soft nil)))
(defparameter *server* nil)

(hunchentoot:define-easy-handler (interpret-01 :uri "/api/v01/eat") (textjson)
  (setf (hunchentoot:content-type*) "text/json")
  (when *log-requests-p*
    (log:info "Received a request for processing ~A"
              (if (or (not textjson) (<= (length textjson) 140))
                  textjson ; don't do replacements on short ones, they may be nil
                  (cl-strings:replace-all (subseq textjson 0 140) (format nil "~%") " "))))
  (if (not textjson)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
        (format nil "No legible textjson parameter provided."))
      (let ((period-alists
              (cl-json:encode-json-to-string
               (stationary-analysis-applied
                (cl-json:decode-json-from-string textjson)))))
        ;; For now send them with just the stationary analysis.
        (submit-docs-to-solr period-alists))))

(hunchentoot:define-easy-handler (status-01 :uri "/api/v01/status") (html sourcetype)
  (setf (hunchentoot:content-type*) "text/plain")
  "ok")

;;; Start the server.
(when *server-running-p*
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port *http-port*))
  (when *server-silentp*
    (setf (hunchentoot:acceptor-access-log-destination *server*) nil)
    (setf (hunchentoot:acceptor-message-log-destination *server*) nil))
  (setf hunchentoot:*catch-errors-p* (not *server-enter-debug-p*))
  (hunchentoot:start *server*))
