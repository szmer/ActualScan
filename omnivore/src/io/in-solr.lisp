(in-package :omnivore)

(defun solr-tokens (address port core query)
  ;;; One of general KLUDGE s is that there is no corpus object here.
  "The second value contains statistics got directly from Solr."
  (let* ((http-query (format nil
                               (concatenate 'string
                                            "http://~A:~A/solr/~A/select?q=~A"
                                            ;; KLUDGE to test tags
                                            ;-"%20AND%20tags:entrepreneur"
                                            ;; Get those fields, but no text (only highlights).
                                            "&fl=url,author,title,date_post,date_retr,site_name"
                                            ;; Enable highliting in text.
                                            "&hl=true&hl.fl=text"
                                            ;; Limit the size and number of highlights per doc.
                                            "&hl.fragsize=500&hl.snippets=~A"
                                            ;; Disable marking the highlight.
                                            "&hl.simple.pre=&hl.simple.post="
                                            ;; Sort oldest first (this is important for sentence
                                            ;; deduplication).
                                            "&sort=date_post%20asc"
                                            ;; Set how many rows we want to get.
                                            "&rows=~A"
                                            ;; Faceting - get time information.
                                            "&facet.range=date_post&facet=true"
                                            "&facet.range.start=2010-01-01T00:00:00Z&facet.range.end=NOW"
                                            "&facet.range.gap=%2B1YEAR"
                                            ;; Group by source_type - balance them out
                                            "&group=true&group.field=source_type&group.limit=~A"
                                            ;; Give us a single docs field, instead of separating
                                            ;; the groups
                                            "&group.main=true")
                               address port core query *solr-snippets-per-doc* *solr-total-row-limit*
                               *solr-group-row-limit*))
         (response (timed-execution
                       (babel:octets-to-string
                         (drakma:http-request http-query
                                              ;; this is needed to preserve the encoded space in
                                              ;; sort clause
                                              ;;; see https://github.com/edicl/drakma/issues/78
                                              :preserve-uri t :external-format-in :utf-8))))
         (response-json (timed-execution (yason:parse response)))
         (json-docs (gethash "docs" (or (gethash "response" response-json)
                                        (error (format nil "No response field in ~A~%"
                                                       response)))))
         (hilites (or (gethash "highlighting" response-json)
                      (error (format nil "No highlighting field in ~A~%" response))))
         (result-tokens)
         (sentence-strings-table (make-hash-table :test #'equalp)))
    (format t "~A~%" http-query)
    (when *debug-scoring*
      (format t "~A documents received~%" (length json-docs)))
    (timed-execution
      (dolist (json-doc json-docs)
        (let ((document (make-division :document nil "noid" nil
                                       :title (gethash "title" json-doc)
                                       :creator (gethash "author" json-doc)
                                       :publication-date (gethash "date_post" json-doc)
                                       :meta (alexandria:alist-hash-table
                                               (list (cons "url" (gethash "url" json-doc))
                                                     (cons "site_name"
                                                           (gethash "site_name" json-doc)))
                                               :test #'equalp)))
              (section-strings (reduce
                                 ;; Concatenate all sections extracted from text higlights.
                                 ;;
                                 ;; TODO possible duplication because of this?
                                 #'append
                                 (mapcar
                                   (lambda (text)
                                     (cl-strings:split text
                                                       ;; for some reason including newline as \n
                                                       ;; doesn't work
                                                       (format nil "~%~%")))
                                   (gethash "text" (gethash (gethash "url" json-doc) hilites))))))
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
                  (let ((sentence-as-list (cl-strings:split sentence-string " "))
                        (sentence (make-division :sentence section "noid" nil)))
                    (push sentence (division-divisions section))
                    (dolist (token-string sentence-as-list)
                      (let ((token (make-division :token sentence "noid" token-string)))
                        (push token (division-divisions sentence))
                        (push token result-tokens)))
                    (setf (division-divisions sentence) (reverse (division-divisions sentence))))))
              (setf (division-divisions section) (reverse (division-divisions section)))))
          (setf (division-divisions document) (reverse (division-divisions document)))))
      ;; for timed-execution
      :description "conversion")
    (values
      result-tokens
      (let ((result-list (list :years nil :year-counts nil))
            (year-label-p t))
        (dolist (item (gethash "counts"
                               (gethash "date_post"
                                        (gethash "facet_ranges"
                                                 (gethash "facet_counts"
                                                          response-json)))))
          (if year-label-p
              (progn (push (subseq item 0 4) (getf result-list :years))
                     (setf year-label-p nil))
              ;; TODO scale up for the current year
              (progn (push item (getf result-list :year-counts))
                     (setf year-label-p t))))
        (mapcar (lambda (item) (if (listp item) (reverse item) item))
                result-list)))))
