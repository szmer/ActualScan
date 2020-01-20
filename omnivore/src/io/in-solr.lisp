(in-package :omnivore)

(defun solr-tokens (port core query)
  ;;; One of general KLUDGE s is that there is no corpus object here.
  (let* ((http-query (format nil
                               (concatenate 'string
                                            "http://localhost:~A/solr/~A/select?q=~A"
                                            ;; Enable highliting in text.
                                            "&hl=true&hl.fl=text"
                                            ;; Sort oldest first (this is important for sentence
                                            ;; deduplication).
                                            "&sort=date_post%20asc"
                                            ;; Set how many rows we want to get.
                                            "&rows=50")
                               port core query))
         (response (timed-execution
                       (convert-drakma-to-string
                         (drakma:http-request http-query
                                              ;; this is needed to preserve the encoded space in
                                              ;; sort clause
                                              ;;; see https://github.com/edicl/drakma/issues/78
                                              :preserve-uri t))))
         (response-json (yason:parse response))
         ;; KLUDGE unsafe gethashes
         (json-docs (gethash "docs" (or (gethash "response" response-json)
                                        (error (format nil "No response field in ~A~%"
                                                       response)))))
         (hilites (or (gethash "highlighting" response-json)
                      (error (format nil "No highlighting field in ~A~%" response))))
         (result-tokens)
         (unincd 0)
         (sentence-strings-table (make-hash-table :test #'equalp)))
    (dolist (json-doc json-docs)
      (let ((document (make-division :document nil "noid" nil
                                     :title (gethash "title" json-doc)
                                     :creator (gethash "author" json-doc)
                                     ;; KLUDGE this isn't true, but we use it as mockup for now
                                     :publication-date (gethash "date_retr" json-doc)
                                     :meta (alexandria:alist-hash-table
                                             (list (cons "url" (gethash "url" json-doc)))
                                             :test #'equalp)))
            (document-hilite (cl-ppcre:scan-to-strings
                               "(?<=<em>).*(?=</em>)"
                               (first
                                 (gethash "text"
                                        (gethash (gethash "url" json-doc) hilites)))))
            (docinc)
            (section-strings (cl-strings:split (gethash "text" json-doc)
                                               ;; for some reason including newline as \n doesn't
                                               ;; work
                                               (format nil "~%~%"))))
        (dolist (section-string section-strings)
          (let* ((whitespace-tokenization ; sentence strings; KLUDGE by whitespace
                   (cl-strings:split section-string (format nil "~%")))
                 (section (make-division :section document "noid" nil)))
            (push section (division-divisions document))
            (dolist (sentence-string whitespace-tokenization)
              ;;
              ;; Avoid sentences with duplicate strings.
              ;; This should be mostly safe as long as the docs are in chronological order.
              (unless (gethash (cl-strings:clean sentence-string)
                               sentence-strings-table)
                (setf (gethash (cl-strings:clean sentence-string)
                               sentence-strings-table)
                      t)
                (let ((contains-match-p (truep (search document-hilite sentence-string)))
                      (sentence-as-list (cl-strings:split sentence-string " "))
                      (sentence (make-division :sentence section "noid" nil)))
                  (push sentence (division-divisions section))
                  (dolist (token-string sentence-as-list)
                    (let ((token (make-division :token sentence "noid" token-string)))
                      (push token (division-divisions sentence))
                      (when contains-match-p
                        (setf docinc t)
                        (push token result-tokens))))
                  (setf (division-divisions sentence) (reverse (division-divisions sentence))))))
            (setf (division-divisions section) (reverse (division-divisions section)))))
        (unless docinc (incf unincd))
        (setf (division-divisions document) (reverse (division-divisions document)))))
    (format t "~A docs skipped, can't find the highlights~%" unincd)
    result-tokens))
