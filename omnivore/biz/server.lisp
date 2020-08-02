(in-package :omnivore)

;;;
;;;
(defun sent-alist-representation (sent)
  (list (cons :text (raw-text sent))
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
(hunchentoot:define-easy-handler (query-response :uri "/result") (q sdate edate und)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((query-result
          (apply #'query-result-solr!
                 (concatenate 'list
                              (list q)
                              (when sdate (list :start-date sdate))
                              (when edate (list :end-date edate))
                              (when und (list :undated (not (equalp "0" und))))))))
    (cl-json:encode-json-to-string
      ;; The query result may be nothing if we can't get nothing from Solr, so remember to give
      ;; 0 sents on no information.
     (list (cons :sent-count (or (getf query-result :sent-count) 0))
           (cons :query-text q)
           (cons :typical
                 (mapcar #'sent-alist-representation
                         (getf query-result :typical)))
           (cons :atypical
                 (mapcar #'sent-alist-representation
                         (getf query-result :atypical)))
           (cons :phrases
                 (mapcar (lambda (phrase-entry)
                           (list (cons
                                   :text (cl-strings:join (getf phrase-entry :phrase)
                                                          :separator " "))
                                 (cons
                                   :typical
                                   (mapcar #'sent-alist-representation
                                           (getf phrase-entry :typical)))))
                         (getf query-result :phrases)))
           (cons :sites-stats
                 (mapcar (lambda (site-entry) (list (cons :site (car site-entry))
                                                    (cons :frequency (cdr site-entry))))
                         (getf query-result :sites-stats)))
           (cons :years (getf query-result :years))
           (cons :year-counts (getf query-result :year-counts))))))

;;;
;;; Start the server.
(setf *server* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start *server*)
