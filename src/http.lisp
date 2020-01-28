(in-package :speechtractor)

;;; Clear what may remain from the earlier server objects
(when (boundp '*server*)
      (ignore-errors (hunchentoot:stop *server* :soft nil)))
(defparameter *server* nil)

(hunchentoot:define-easy-handler (interpret-01 :uri "/api/v01/interpret") (html sourcetype)
  (setf (hunchentoot:content-type*) "text/json")
  (html-document-data-json html (gethash sourcetype *source-type-meta-funs*
                                         ;; apparently we cannot put an error call here directly
                                         ;; b/c it would be evaluated.
                                         (format nil "no meta-funs for sourcetype ~S"
                                                 sourcetype))
                           :classification-settings
                           (gethash sourcetype *source-type-classification-settings*
                                    (format nil "no classif. settings for sourcetype ~S"
                                            sourcetype))
                           :split-sents t))

;;; Start the server.
(when *server-running-p*
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port *http-port*))
  (when *server-silentp*
    (setf (hunchentoot:acceptor-access-log-destination *server*) nil)
    (setf (hunchentoot:acceptor-message-log-destination *server*) nil))
  (hunchentoot:start *server*))
