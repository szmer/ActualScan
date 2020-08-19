(in-package :speechtractor)

(defun parsed-natural-date (date-string)
  "Get an object accepted by local-time formatter from a string with date description."
  (chronicity:parse
    ;; NOTE strings have to be trimmed, due to some bug in chronicity!
    (string-trim '(#\Space) date-string)
    ;; All our dates should be in the past; get start of the span on ambiguity
    :context :past :guess :start))

;;; TODO test
(defun solr-date-str (date-time)
  "Return a string with datetime formatted that is digestible by Solr."
  (when (typep date-time 'local-time:timestamp)
    (local-time:format-timestring
     nil ;; null destination as with the CL format function
     date-time
     :format '((:year 4) "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z"))))

(defun maybe-solr-date-from-str (date-string)
  (solr-date-str (parsed-natural-date date-string)))

(defun forums-date (node path)
  (cond
    ;; some old ver of phpBB (redflagdeals)
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "dateline_timestamp") (plump:attribute node "class")))
     (maybe-solr-date-from-str (plump:render-text node)))
    ;; some Xenforo (styleforum)
;;;;- This is not a very good idea, but instructive stuff:
;;;;-    ((and (plump:has-attribute node "data-lb-caption-desc")
;;;;-          ;; we would have to escape & in &middot;, as cl-ppcre interprets it as a variable, but it
;;;;-          ;; seems to be parsed to unicode anyway
;;;;-          (cl-ppcre:scan "·" (plump:attribute node "data-lb-caption-desc")))
;;;;-     (maybe-solr-date-from-str (cl-ppcre:scan-to-strings "(?<=·).*"
;;;;-                                            (plump:attribute node "data-lb-caption-desc"))))
    ((and (equalp "time" (plump:tag-name node))
          (plump:has-attribute node "class")
          (plump:has-attribute node "title")
          (cl-ppcre:scan (boundary-regex "u-dt") (plump:attribute node "class"))
          ;; take only inside links, as it also occurs in other places
          (equalp '("a" "time") (last path 2)))
     (maybe-solr-date-from-str (plump:attribute node "title")))
    ;; Xenforo head-fi (horrible!)
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "item") (plump:attribute node "class"))
          (or (search "AM" (plump:render-text node))
              (search "PM" (plump:render-text node)))
          (typep node 'plump:child-node)
          (typep (plump:parent node) 'plump:child-node)
          (plump:has-attribute (plump:parent (plump:parent node)) "class")
          (cl-ppcre:scan (boundary-regex "messageUserInfo")
                         (plump:attribute (plump:parent (plump:parent node))
                                        "class")))
     (let ((text (plump:render-text node)))
       ;; Throw out the stuff that may appear after the date.
       (multiple-value-bind (start end)
         (cl-ppcre:scan "(AM|PM)" text)
         (maybe-solr-date-from-str (subseq text 0 end)))))
    ;; Xenforo fashionspot
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "datePermalink") (plump:attribute node "class")))
     (maybe-solr-date-from-str (plump:render-text node)))
    ;; some Wordpress forums
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "post-time") (plump:attribute node "class")))
     (maybe-solr-date-from-str (plump:render-text node)))
    ;; thestudentroom
    ((and (equalp "span" (plump:tag-name node))
          (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "timestamp") (plump:attribute node "class"))
          (typep node 'plump:child-node)
          (plump:has-attribute (plump:parent node) "class")
          (cl-ppcre:scan (boundary-regex "post-subheader")
                         (plump:attribute (plump:parent node) "class")))
     (maybe-solr-date-from-str (plump:render-text node)))
    ;; Wallstreetoasis
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "post-when") (plump:attribute node "class")))
     ;; NOTE Chronicity cannot handle hyphens for some reason
     (maybe-solr-date-from-str (cl-strings:replace-all (plump:render-text node) "-" "")))))

;;
;; TODO on debugging it would be nice to get the trace and processed value on NIL!
(defun media-date (node path)
  (cond ((and (plump:has-attribute node "itemprop")
              (equalp "datePublished" (plump:attribute node "itemprop")))
         ;; It's already in the RFC format, so local-time will handle it better than chronicity
         (solr-date-str (or
                          (local-time:parse-timestring ; on Blogspot
                            (plump:attribute node "title") :fail-on-error nil)
                          (local-time:parse-timestring
                           (plump:attribute node "datetime") :fail-on-error nil))))
        ;; Wordpress entry.
        ((and (equalp "time" (plump:tag-name node))
              (plump:has-attribute node "class")
              (cl-ppcre:scan "entry-date" (plump:attribute node "class")))
         (solr-date-str
          (local-time:parse-timestring (plump:attribute node "datetime") :fail-on-error nil)))
        ;; This is in Glamour - probably better to extract this from somewhere in the JSON metadata
        ;; soup (the actual class is content-header__publish-date)
        ((and (equalp "time" (plump:tag-name node))
              (plump:has-attribute node "class")
              (cl-ppcre:scan "publish-date" (plump:attribute node "class")))
         (maybe-solr-date-from-str (plump:render-text node)))
        ;; CNN
        ((and (equalp "div" (plump:tag-name node))
              (plump:has-attribute node "class")
              (cl-ppcre:scan "[Pp]ublished" (plump:attribute node "class")))
         (maybe-solr-date-from-str (cl-strings:replace-all (plump:render-text node) "Updated" "")))
        ;; Whatever Gates Notes use.
        ((and (plump:has-attribute node "class")
              (cl-ppcre:scan (boundary-regex "article_top_dateline") (plump:attribute node "class")))
         (maybe-solr-date-from-str (plump:render-text node)))))
