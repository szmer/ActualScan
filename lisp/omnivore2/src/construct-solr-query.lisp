(in-package :omnivore2)

(defun solr-query-field-from-period-alist (period-alist)
  "Given a period alist, create a string for Solr (main) query for context for this text period."
  (format nil "q=tags:~A" (cdr :tags period-alist)))

(defun solr-boost-field-from-period-alist (period-alist)
  "Given a period alist, create an alist corresponding to a JSON Solr bf (boost function) query for\
context for this text period."
  (let ((field-parts))
    (dolist (entry period-alist)
      (when (not (find (car entry) '(:date--retr :date--class)))
        (cond ((numberp (cdr entry))
               ;; there is a problem of diffrent ranges/scales which we ignore for now
               (push (format nil "recip(abs(~a-(~a)),1,1000,1000)"
                             (cl-json:encode-json-to-string (car entry))
                             (cdr entry))
                     field-parts))
              ((cl-ppcre:all-matches "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$" (cdr entry))
               ;; For times, get the difference in ms and use a bigger scale.
               (push (format nil "recip(abs(ms(~a, ~a)),1,100000000,100000000)"
                             (cl-json:encode-json-to-string (car entry)) (cdr entry))
                     field-parts)))))
    (concatenate 'string
                 "bf="
                 ;; Adding is probably less informative, but less dangerous in terms of underflows
                 (cl-strings:join field-parts :separator " + "))))
