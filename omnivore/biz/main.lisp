(in-package :omnivore)

(defun result-for-tokens (tv-tokens)
  "Get a readable, but sexp-structured result for tv-view containing tokens as divisions, with\
   the documents loaded."
  (let ((tv-sentences (timed-execution (tokens-sents-with-windows 2 tv-tokens :deduplicate t))))
    (when *debug-scoring*
      (format t "Working on ~A sentences~%" (length tv-sentences)))
    (when tv-sentences
      (list :typical (timed-execution (typical tv-sentences 10))
        :atypical (timed-execution (atypical tv-sentences 10))
        :sites-stats (timed-execution (sites-stats tv-sentences))
        :phrases (timed-execution
                   (phrases-info tv-sentences
                                 (max 5
                                      (ceiling (* *phrase-freq-threshold* (length tv-sentences))))
                                 *phrase-example-count*
                                 :sents-to-text nil))))))

(defun query-result (query corpus)
  (let* ((category (make-category :token :attribute-name "raw_text"
                                  :attribute-value query :criterion :equal))
         (view (pg-textviews:get-view corpus (list category) :get-documents t)))
    (result-for-tokens (view-divisions tv-view))))

(defun query-result-solr! (query)
  (let* ((tv-tokens (solr-tokens *solr-port* *solr-collection*
                                 ;; enclose in url-encoded quotation marks
                                 (format nil "text:%22~A%22" (drakma:url-encode query :utf-8)))))
    (result-for-tokens tv-tokens)))
