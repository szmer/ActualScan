(in-package :omnivore)

(defun result-for-tokens (tv-tokens)
  "Get a readable, but sexp-structured result for tv-view containing tokens as divisions, with\
   the documents loaded."
  (let ((tv-sentences  (tokens-sents-with-windows 2 tv-tokens)))
    (when tv-sentences
      (list :typical (typical tv-sentences 10)
        :atypical (atypical tv-sentences 10)
        :phrases (phrases-info tv-sentences 10 3 :sents-to-text nil))
      )))

(defun query-result (query corpus)
  (let* ((category (make-category :token :attribute-name "raw_text"
                                  :attribute-value query :criterion :equal))
         (view (pg-textviews:get-view corpus (list category) :get-documents t)))
    (result-for-tokens (view-divisions tv-view))))

(defun query-result-solr! (query)
  (let* ((tv-tokens (solr-tokens *solr-port* *solr-collection* (format nil "text:~A" query))))
    (result-for-tokens tv-tokens)))
