(in-package :omnivore)

(defun solr-tokens (address port core solr-query &key (start-date nil) (end-date nil) (undated nil)
                            (sites nil))
  "Sites should be provided space-separated as one string. Start, end date as Solr date strings.\
   Values: loose tokens with sentence references, plist of additional stats (years, document counts\
   for the year."
  ;;; One of general KLUDGE s is that there is no corpus object here.
  "The second value contains statistics got directly from Solr."
  (assert (not (and end-date (not start-date))))
  (let* ((http-query (format nil
                               (concatenate 'string
                                            "http://~A:~A/solr/~A/select?q=~A"
                                            ;; note that fq can be faster than q
                                            ;; Set the start and end dates.
                                            (if start-date
                                                (concatenate
                                                  'string
                                                  ;; NOTE it has to be in a format recognized by Solr
                                                  ;; (datestamp or some verbal descriptions)
                                                  ;; %5B %5D = [ ]
                                                  (format nil "&fq=date_post:%5B~a%20TO%20~a%5D"
                                                        start-date
                                                        (or end-date "NOW"))
                                                  ;; apparently that's how you detect empty fields
                                                  ;; in AND/OR constructions
                                                  (if undated "%20OR%20(*:*%20AND%20-date_post:%5B*%20TO%20*%5D)"
                                                      ""))
                                                "")
                                            ;; Set the desired sites.
                                            (if sites
                                                (format nil "&fq=site_name:(~A)"
                                                        (cl-strings:replace-all
                                                          (cl-strings:join
                                                            (mapcar (lambda (name)
                                                                      ;; quotes
                                                                      (format nil "%22~A%22" name))
                                                                    (cl-strings:split sites " "))
                                                            ;; encoded space
                                                            :separator "%20")
                                                          ;; Solr has a problem with slashes.
                                                          "/" "%5C%2F"))
                                                "")
                                            ;; Get those fields, but no text (only highlights).
                                            "&fl=url,author,title,date_post,date_retr,site_name"
                                            ;; Enable highliting in text.
                                            "&hl=true&hl.fl=text"
                                            ;; Limit the size and number of highlights per doc.
                                            "&hl.fragsize=~A&hl.snippets=~A"
                                            ;; Disable marking the highlight.
                                            "&hl.simple.pre=&hl.simple.post="
                                            ;; Sort oldest first (this is important for sentence
                                            ;; deduplication).
                                            "&sort=date_post%20asc"
                                            ;; Set how many rows we want to get.
                                            "&rows=~A"
                                            ;; Faceting - get time information.
                                            "&facet.range=date_post&facet=true"
                                            (format
                                              nil
                                              "&facet.range.start=~A&facet.range.end=~A"
                                              (or start-date "2010-01-01T00:00:00Z")
                                              (or end-date "NOW"))
                                            "&facet.range.gap=%2B1MONTH"
                                            ;; Group by source_type - balance them out
                                            "&group=true&group.field=site_name&group.limit=~A"
                                            ;; Give us a single docs field, instead of separating
                                            ;; the groups
                                            "&group.main=true")
                               address port core solr-query *solr-hl-fragsize* *solr-snippets-per-doc*
                               *solr-total-row-limit* *solr-group-row-limit*))
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
    (when *debug-solr-connection* (format t "~A~%" http-query))
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
          (if year-label-p ; years alternate with counts in Solr results
              (progn (push (subseq item 0 7) ; extract only the year from the string
                           (getf result-list :years))
                     (setf year-label-p nil))
              ;; TODO scale up for the current year
              (progn (push item (getf result-list :year-counts))
                     (setf year-label-p t))))
        (mapcar (lambda (item) (if (listp item) (reverse item) item))
                result-list))))) 
