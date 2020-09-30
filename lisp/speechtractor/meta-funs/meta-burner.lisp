(in-package :speechtractor)

;;;; The meta burner function gets access to whole nodes refused due to skip-p or skipped-tags,
;;;; it can return a property list that is assigned into the document metadata.

(defun general-meta-burner (node path)
  (when (plump:element-p node)
    (let ((output-plist)
          (json-ld-script-tag
            (or (when (and (equalp "script" (plump:tag-name node))
                           (equalp "application/ld+json" (plump:attribute node "type"))
                           (search "schema.org" (plump:render-text node)))
                  node)
                (find-if (lambda (element)
                           (and (equalp "application/ld+json" (plump:attribute element "type"))
                                (search "schema.org" (plump:render-text element))))
                         (plump:get-elements-by-tag-name node "script"))))
          (author-blog-tag (find-if ;; A Wordpress thing, in entry <footer>
                                    (lambda (element)
                                      (cl-ppcre:scan (boundary-regex "entry-author-name")
                                                     (plump:attribute element "class")))
                                    (plump:get-elements-by-tag-name node "span")))
          (author-meta-node
            (cond ((and (equalp "meta" (plump:tag-name node))
                        (or
                          (equalp "author" (plump:attribute node "name"))
                          (equalp "author" (plump:attribute node "property"))
                          (equalp "article:author" (plump:attribute node "property")))))
                  (t (find-if
                             (lambda (element)
                               (or
                                 (equalp "author" (plump:attribute element "name"))
                                 (equalp "author" (plump:attribute element "property"))
                                 (equalp "article:author" (plump:attribute element "property"))))
                             (plump:get-elements-by-tag-name node "meta")))))
          (date-meta-node
            (cond ((and (equalp "meta" (plump:tag-name node))
                        (equalp "article:published_time" (plump:attribute node "property"))))
                  (t (find-if
                             (lambda (element)
                               (equalp "article:published_time"
                                       (plump:attribute element "property")))
                             (plump:get-elements-by-tag-name node "meta"))))))
      (when json-ld-script-tag
        (let ((schema-org-script (cl-json:decode-json-from-string (plump:render-text json-ld-script-tag))))
          (setf (getf output-plist :author)
                (if (listp (cdr (assoc :author schema-org-script)))
                    (cdr (assoc :name (cdr (assoc :author schema-org-script))))
                    (cdr (assoc :author schema-org-script))))
          (setf (getf output-plist :date_post)
                (solr-date-str
                 (local-time:parse-timestring
                  (cdr (assoc :date-published schema-org-script)) :fail-on-error nil)))))
      (cond (author-blog-tag (setf (getf output-plist :author)
                                   (plump:render-text author-blog-tag)))
            ((and author-meta-node
                  (rejecting-when-true ; check if not nil to avoid replacing a good value
                   (lambda (x) (ignore-errors (purl:url x))) ; reject urls as author meta
                   (html-stripped (plump:attribute author-meta-node "content"))))
             (setf (getf output-plist :author)
                   (rejecting-when-true
                    (lambda (x) (ignore-errors (purl:url x)))
                    (html-stripped (plump:attribute author-meta-node "content"))))))
      (when date-meta-node (setf (getf output-plist :date_post)
                                 (solr-date-str
                                   (local-time:parse-timestring
                                     (plump:attribute date-meta-node "content") :fail-on-error nil))))
      output-plist)))
