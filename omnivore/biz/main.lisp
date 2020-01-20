(in-package :omnivore)

(defun result-for-tokens (tv-tokens)
  "Get a readable, but sexp-structured result for tv-view containing tokens as divisions, with\
   the documents loaded."
  (let ((tv-sentences (timed-execution (tokens-sents-with-windows 2 tv-tokens :deduplicate t))))
    (format t "Working on ~A sentences~%" (length tv-sentences))
    (when tv-sentences
      (list :typical (timed-execution (typical tv-sentences 10))
        :atypical (timed-execution (atypical tv-sentences 10))
        :phrases (timed-execution (phrases-info tv-sentences 3 3 :sents-to-text nil))))))

(defun query-result (query corpus)
  (let* ((category (make-category :token :attribute-name "raw_text"
                                  :attribute-value query :criterion :equal))
         (view (pg-textviews:get-view corpus (list category) :get-documents t)))
    (result-for-tokens (view-divisions tv-view))))

(defun query-result-solr! (query)
  (let* ((tv-tokens (solr-tokens *solr-port* *solr-collection*
                                 (format nil "text:~A" (drakma:url-encode query :utf-8)))))
    (result-for-tokens tv-tokens)))
