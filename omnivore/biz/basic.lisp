(in-package :omnivore)

(defun typical (tv-sentences n &key (strip-scores t))
    (when *debug-scoring*
      (format t "Looking for typical sentences.~%"))
  (mapcar (if strip-scores #'first #'identity)
          (cond ((< (length tv-sentences) (* 5 n))
                 ;; If there are few sentences, don't discriminate.
                 (ranked-low
                   (scored-with-average-tfidf tv-sentences)
                   :n n))
                ((< (length tv-sentences) (* 10 n))
                 (ranked-low
                   ;; The intermediate case: take half.
                   (lowest-chunk 0.5
                                 (scored-with-average-tfidf tv-sentences)
                                 (scored-with-length-deviation tv-sentences))
                   :n n))
                (t
                 ;; When there's a lot, take ten times we need.
                 (ranked-low
                   (lowest-chunk (/ (* 10 n) (length tv-sentences))
                                 (scored-with-average-tfidf tv-sentences)
                                 (scored-with-length-deviation tv-sentences))
                   :n n)))))

;;; TODO we want to ignore special characters in these calculations.
(defun atypical (tv-sentences n &key (strip-scores t))
    (when *debug-scoring*
      (format t "Looking for atypical sentences.~%"))
  (mapcar (if strip-scores #'first #'identity)
          (ranked-high (corrected-with #'*
                               (corrected-with #'/
                                               (scored-with-average-tfidf tv-sentences)
                                               (scored-with-length-deviation tv-sentences))
                               (scored-with-markers tv-sentences))
               :n n)))

(defun phrases-info (tv-sentences freq num-examples &key (sents-to-text t) (give-atypical nil))
  (when *debug-scoring*
    (format t "-----Looking for phrases...-----~%"))
  (let ((bigrams (top-bigrams tv-sentences :freq freq))
        ;; NOTE trigrams are slow and currently is hard for them to even meaningfully appear
       ;;- (trigrams (top-trigrams tv-sentences :freq (max 3 (floor (/ freq 3)))))
        )
    (mapcar (lambda (phrase-entry)
              (let* ((phrase (car phrase-entry)) ; the cdr is frequency
                     (containing-sents (sentences-with-ngram phrase tv-sentences)))
                (when *debug-scoring*
                  (format t "---phrase: ~A---~%" phrase))
                (append
                  (list :phrase phrase
                        :typical
                        (mapcar (if sents-to-text #'raw-text #'identity)
                                (typical containing-sents num-examples)))
                  (when give-atypical
                    (list :atypical (mapcar (if sents-to-text #'raw-text #'identity)
                                                   (atypical containing-sents num-examples)))))))
            ;; Sort - give the most frequent ones first.
            (sort (append (alexandria:hash-table-alist bigrams)
                         ;;- (alexandria:hash-table-alist trigrams)
                          )
                  #'> 
                  :key #'cdr))))
