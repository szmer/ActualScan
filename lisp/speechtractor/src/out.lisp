(in-package :speechtractor)

(defun html-document-data-json (html-string metadata-funs
                                            &key (classification-settings (make-hash-table))
                                            (paragraph-separator (format nil "~%~%"))
                                            (remove-if-empty-url nil))
  (multiple-value-bind (paragraphs docs-metadata metadata-log)
    (html-document-data html-string metadata-funs
                        :classification-settings classification-settings)
    (let ((paragraph-n 0)
          (result-docs))
      (dolist (doc-metadata docs-metadata)
        (setf (getf doc-metadata :text) "")
        (push
          (do ((doc-foundp)
               (paragraph (nth paragraph-n paragraphs) (nth paragraph-n paragraphs))
               (doc-data doc-metadata))
              ((or (null paragraph)
                   (and doc-foundp (paragraph-doc-startp paragraph)))
               doc-data)
            ;; Collect the paragraphs into the document plist text property.
              (incf paragraph-n)
              (when (paragraph-doc-startp paragraph) (setf doc-foundp t))
              (when (and doc-foundp (eq :good (paragraph-classification paragraph)))
                (let ((text (paragraph-text paragraph :cleanp t)))
                  (setf (getf doc-metadata :text)
                        (concatenate 'string (getf doc-metadata :text)
                                     (if (not (or (zerop (length (getf doc-metadata :text)))
                                                  ;; avoid spacing empty paragraphs
                                                  (zerop (length text))))
                                         paragraph-separator
                                         "")
                                     (cleaned-text text))))))
          result-docs))
      (when remove-if-empty-url
        (setf result-docs (remove-if (lambda (doc) (not (getf doc :url)))
                                     result-docs)))
      ;; cl-json expects alists, we have property lists
      (let ((response (cl-json:encode-json-to-string
                        (mapcar #'alexandria:plist-alist (reverse result-docs)))))
        (if (equalp response "null")
        "[]"
        response)))))
