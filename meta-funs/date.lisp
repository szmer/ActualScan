(in-package :speechtractor)

(defun parsed-natural-date (date-string)
  (chronicity:parse
    ;; NOTE strings have to be trimmed, due to some bug in chronicity!
    (string-trim '(#\Space) date-string)
    ;; All our dates should be in the past; get start of the span on ambiguity
    :context :past :guess :start))

;;; TODO test
(defun solr-date-str (date-time)
  "Return a string with datetime formatted that is digestible by Solr."
  (local-time:format-timestring
    nil date-time
    :format '((:year 4) ":" (:month 2) ":" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z")))

(defun date-solr-str (date-string)
  (solr-date-str (parsed-natural-date date-string)))

(defun forums-date (node path)
  (cond
    ;; some old ver of phpBB (redflagdeals)
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "dateline_timestamp") (plump:attribute node "class")))
     (date-solr-str (plump:render-text node)))
    ;; some Xenforo (styleforum)
;;;;- This is not a very good idea, but instructive stuff:
;;;;-    ((and (plump:has-attribute node "data-lb-caption-desc")
;;;;-          ;; we would have to escape & in &middot;, as cl-ppcre interprets it as a variable, but it
;;;;-          ;; seems to be parsed to unicode anyway
;;;;-          (cl-ppcre:scan "·" (plump:attribute node "data-lb-caption-desc")))
;;;;-     (date-solr-str (cl-ppcre:scan-to-strings "(?<=·).*"
;;;;-                                            (plump:attribute node "data-lb-caption-desc"))))
    ((and (equalp "time" (plump:tag-name node))
          (plump:has-attribute node "class")
          (plump:has-attribute node "title")
          (cl-ppcre:scan (boundary-regex "u-dt") (plump:attribute node "class"))
          ;; take only inside links, as it also occurs in other places
          (equalp '("a" "time") (last path 2)))
     (date-solr-str (plump:attribute node "title")))
    ;; some Wordpress forums
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "post-time") (plump:attribute node "class")))
     (date-solr-str (plump:render-text node)))
    ;; thestudentroom
    ((and (equalp "span" (plump:tag-name node))
          (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "timestamp") (plump:attribute node "class"))
          (typep node 'plump:child-node)
          (plump:has-attribute (plump:parent node) "class")
          (cl-ppcre:scan (boundary-regex "post-subheader")
                         (plump:attribute (plump:parent node) "class")))
     (date-solr-str (plump:render-text node)))
    ;; Wallstreetoasis
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "post-when") (plump:attribute node "class")))
     ;; NOTE Chronicity cannot handle hyphens for some reason
     (date-solr-str (cl-strings:replace-all (plump:render-text node) "-" "")))))
