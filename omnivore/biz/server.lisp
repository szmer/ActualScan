(in-package :omnivore)

;;;
;;;
(defun sent-alist-representation (sent)
  (list (cons :sent-text (raw-text sent))
        (cons :date
              (if (read-attribute sent "publication-date")
                  ;; KLUDGE poor man's formatting
                  (subseq (read-attribute sent "publication-date") 0 10)
                  ""))
        (cons :author
              (format nil "~A on ~A"
                             (or (read-attribute sent "creator") "someone")
                             (read-attribute sent "site_name")))
        (cons :url (read-attribute sent "url"))
        (cons :domain (when (read-attribute sent "url")
                        (purl:url-host (purl:url (read-attribute sent "url")))))))

;;; Clear what may remain from the earlier server objects
(when (and (boundp '*server*) *server*) (hunchentoot:stop *server* :soft nil))
(defparameter *server* nil)

;;;
;;; Dynamic responders.
(hunchentoot:define-easy-handler (query-response :uri "/result") (q)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((query-result (query-result-solr! q)))
    (cl-json:encode-json-to-string
     (list (cons :query-text q)
           (list :typical
                 (mapcar #'sent-alist-representation
                         (getf query-result :typical)))
           (list :atypical
                 (mapcar #'sent-alist-representation
                         (getf query-result :atypical)))
           (list :phrases
                 (mapcar (lambda (phrase-entry)
                           (list :phrase (cl-strings:join (getf phrase-entry :phrase)
                                                          :separator " ")
                                 :typical
                                 (mapcar #'sent-alist-representation
                                         (getf phrase-entry :typical))))
                         (getf query-result :phrases)))
           (list :sites-stats
                 (mapcar (lambda (site-entry) (list (cons :site (car site-entry))
                                                    (cons :frequency (cdr site-entry))))
                         (getf query-result :sites-stats)))
           (cons :years (getf query-result :years))
           (cons :year-counts (getf query-result :year-counts))))))

;;;
;;; Start the server.
(setf *server* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start *server*)
