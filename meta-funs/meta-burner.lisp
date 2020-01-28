(in-package :speechtractor)

;;;; The meta burner function gets access to whole nodes refused due to skip-p or skipped-tags,
;;;; it can return a property list that is assigned into the document metadata.

(defun general-meta-burner (node path)
  (when (plump:element-p node)
    (let ((output-plist)
          (author-meta-node
            (cond ((and (equalp "meta" (plump:tag-name node))
                        (equalp "author" (plump:attribute node "name"))
                        (equalp "author" (plump:attribute node "property"))))
                  (t (find-if
                             (lambda (elem)
                               (or
                                 (equalp "author" (plump:attribute elem "name"))
                                 (equalp "author" (plump:attribute elem "property"))))
                             (plump:get-elements-by-tag-name node "meta"))))))
      (when author-meta-node (setf (getf output-plist :author)
                                   (plump:attribute author-meta-node "content")))
      output-plist)))
