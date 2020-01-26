(in-package :speechtractor)

(defun html-document-data-json (html-string metadata-funs
                                            &key (classification-settings (make-hash-table))
                                            (paragraph-separator (format nil "~%~%")))
  (multiple-value-bind (paragraphs docs-metadata)
    (html-document-data html-string metadata-funs
                        :classification-settings classification-settings)
    (let ((paragraph-n 0)
          (result-docs))
      (dolist (doc-metadata docs-metadata)
        (setf (getf doc-metadata :text) "")
        (push
          (do ((doc-found)
               (paragraph (nth paragraph-n paragraphs) (nth paragraph-n paragraphs))
               (doc-data doc-metadata))
              ((or (null paragraph)
                   (and doc-found (paragraph-doc-startp paragraph)))
               doc-data)
              (incf paragraph-n)
              (when (paragraph-doc-startp paragraph) (setf doc-found t))
              (when (eq :good (paragraph-classification paragraph))
                (setf (getf doc-metadata :text)
                      (concatenate 'string (getf doc-metadata :text)
                                   (if (not (zerop (length (getf doc-metadata :text))))
                                       paragraph-separator
                                       "")
                                   (paragraph-text paragraph :cleanp t)))))
          result-docs))
      ;; cl-json expects alists, we have property lists
      (cl-json:encode-json-to-string
        (mapcar #'alexandria:plist-alist (reverse result-docs))))))
