(in-package :speechtractor)

(defun parsed-natural-date (date-string)
  ;; All our dates should be in the past; get start of the span on ambiguity
  (chronicity:parse date-string :context :past :guess :start))

;;; TODO test
(defun solr-date-str (date-time)
  "Return a string with datetime formatted that is digestible by Solr."
  (local-time:format-timestring
    nil date-time
    :format '((:year 4) ":" (:month 2) ":" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z")))

(defun date-object (date-string)
  (solr-date-str (parsed-natural-date date-string)))

(defun forums-date (node path)
  (cond
    ;; some old ver of phpBB
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan "\\bdateline_timestamp\\b" (plump:attribute node "class")))
     (date-object (plump:render-text node)))))
