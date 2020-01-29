(in-package :speechtractor)

;;;; The meta burner function gets access to whole nodes refused due to skip-p or skipped-tags,
;;;; it can return a property list that is assigned into the document metadata.

(defun general-meta-burner (node path)
  (when (plump:element-p node)
    (let ((output-plist)
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
      (cond (author-blog-tag (setf (getf output-plist :author)
                                   (plump:render-text author-blog-tag)))
            (author-meta-node (setf (getf output-plist :author)
                                    (plump:attribute author-meta-node "content"))))
      (when date-meta-node (setf (getf output-plist :date_post)
                                 (solr-date-str
                                   (local-time:parse-timestring
                                     (plump:attribute date-meta-node "content") :fail-on-error t))))
      output-plist)))
