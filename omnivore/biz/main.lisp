(in-package :omnivore)

(defun result-for-tokens (tv-tokens solr-stats)
  "Get a readable, but sexp-structured result for tv-view containing tokens as divisions, with\
   the documents loaded."
  (let ((tv-sentences (timed-execution (tokens-sents-with-windows 2 tv-tokens :deduplicate t))))
    (when *debug-scoring*
      (format t "Working on ~A sentences~%" (length tv-sentences)))
    (when tv-sentences
      (list :sent-count (length tv-sentences)
            :typical (timed-execution (typical tv-sentences 10))
            :atypical (timed-execution (atypical tv-sentences 10))
            :top-terms (tokens-frequency-list tv-tokens :cutoff (max 3 (/ (length tv-tokens) 800)))
            ;; This gives more flat list of semi-obscure phrases like E.T., p.m., Chatham
            ;; (top-terms-by-tf-idfs (tf-idfs-for-sections (tokens-sections tv-tokens)))
            ;; TODO get those directly from Solr with additional faceting?
            :sites-stats (timed-execution (sites-stats tv-sentences))
            :phrases (timed-execution
                      (phrases-info tv-sentences
                                    (max 5 ; the minimum phrase frequency
                                         (ceiling (* *phrase-freq-threshold* (length tv-sentences))))
                                    *phrase-example-count*
                                    :sents-to-text nil
                                    :give-oldest 1))
            :years (getf solr-stats :years)
            :year-counts (getf solr-stats :year-counts)))))

;;-(defun query-result (query corpus)
;;-  (let* ((category (make-category :token :attribute-name "raw_text"
;;-                                  :attribute-value query :criterion :equal))
;;-         (view (pg-textviews:get-view corpus (list category) :get-documents t)))
;;-    (result-for-tokens (view-divisions tv-view))))

(defun query-result-solr! (query &key (start-date nil) (end-date nil) (undated nil) (sites nil))
  (multiple-value-bind (tv-tokens solr-stats)
    (solr-tokens *solr-address* *solr-port* *solr-collection*
                 ;; enclose in url-encoded quotation marks
                 (format nil "text:%22~A%22" (drakma:url-encode query :utf-8))
                 :start-date start-date :end-date end-date :undated undated :sites sites)
    (result-for-tokens tv-tokens solr-stats)))
