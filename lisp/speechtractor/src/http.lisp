(in-package :speechtractor)

;;; Clear what may remain from the earlier server objects
(when (boundp '*server*)
      (ignore-errors (hunchentoot:stop *server* :soft nil)))
(defparameter *server* nil)

(hunchentoot:define-easy-handler (interpret-01 :uri "/api/v01/interpret") (html sourcetype emptyurl)
  (multiple-value-bind (user password) (hunchentoot:authorization)
    (if (not (find (list user password) *auth-pairs* :test #'equalp))
        (hunchentoot:require-authorization "speechtractor") ; if auth failed
        (progn
          (setf (hunchentoot:content-type*) "text/json")
          (when *log-requests-p*
            (log:info "Received a request for processing ~A, source type ~A"
                      (if (or (not html) (<= (length html) 140))
                          html ; don't do replacements on short ones, they may be nil
                          (cl-strings:replace-all (subseq html 0 140) (format nil "~%") " "))
                      sourcetype))
          (if (not (and html sourcetype))
              (progn
                (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
                (format nil "Cannot interpret content: html: ~A sourcetype: ~A" html sourcetype))
              (html-document-data-json html (gethash sourcetype *source-type-meta-funs*
                                                     ;; apparently we cannot put an error call here directly
                                                     ;; b/c it would be evaluated.
                                                     (format nil "no meta-funs for sourcetype ~S"
                                                             sourcetype))
                                       :classification-settings
                                       (gethash sourcetype *source-type-classification-settings*
                                                (format nil "no classif. settings for sourcetype ~S"
                                                        sourcetype))
                                       :remove-if-empty-url (not (equalp emptyurl "1"))))))))

(hunchentoot:define-easy-handler (status-01 :uri "/api/v01/status") (html sourcetype)
  (setf (hunchentoot:content-type*) "text/plain")
  "ok")

;;; Start the server.
(when *server-running-p*
  (setf *server* (make-instance 'hunchentoot:easy-ssl-acceptor :port *http-port*
                                :ssl-certificate-file "/home/certs/ascan_internal.pem"
                                :ssl-privatekey-file "/home/certs/ascan_internal_key.key"))
  (when *server-silentp*
    (setf (hunchentoot:acceptor-access-log-destination *server*) nil)
    (setf (hunchentoot:acceptor-message-log-destination *server*) nil))
  (setf hunchentoot:*catch-errors-p* (not *server-enter-debug-p*))
  (hunchentoot:start *server*))
