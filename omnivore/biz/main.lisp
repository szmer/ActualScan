(in-package :omnivore)

(defun result-for-tokens-view (tv-view)
  "Get a readable, but sexp-structured result for tv-view containing tokens as divisions, with\
   the documents loaded."
  (let ((tv-sentences  (tokens-sents-with-windows 2 (view-divisions tv-view))))
  (list (list "typical" (mapcar #'raw-text (typical tv-sentences 10)))
        (list "atypical" (mapcar #'raw-text (atypical tv-sentences 10)))
        (list "phrases" (phrases-info tv-sentences 10 3)))))

(defun query-result (query corpus)
  (let* ((category (make-category :token :attribute-name "raw_text"
                                  :attribute-value query :criterion :equal))
         (view (pg-textviews:get-view corpus (list category) :get-documents t)))
    (result-for-tokens-view view)))
