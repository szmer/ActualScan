(in-package :omnivore)

(defun solr-tokens (port core query)
  ;;; One of general KLUDGE s is that there is no corpus object here.
  (let* ((http-query (format nil "http://localhost:~A/solr/~A/select?q=~A&hl=true&hl.fl=text&rows=50"
                             port core query))
         ;; KLUDGE drakma can return a stream
         (response (convert-drakma-to-string (drakma:http-request http-query)))
         (response-json (yason:parse response))
         ;; KLUDGE unsafe gethashes
         (json-docs (gethash "docs" (gethash "response" response-json)))
         (hilites (gethash "highlighting" response-json))
         (result-tokens)
         (unincd 0))
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
          (let* ((whitespace-tokenization ; KLUDGE
                   (cl-strings:split section-string (format nil "~%")))
                 (section (make-division :section document "noid" nil)))
            (push section (division-divisions document))
            (dolist (sentence-string whitespace-tokenization)
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
                (setf (division-divisions sentence) (reverse (division-divisions sentence)))))
            (setf (division-divisions section) (reverse (division-divisions section)))))
        (unless docinc (incf unincd))
        (setf (division-divisions document) (reverse (division-divisions document)))))
    (format t "~A docs skipped, can't find the highlights~%" unincd)
    result-tokens))
