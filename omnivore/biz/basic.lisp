(in-package :omnivore)

(defun typical (tv-sentences n &key (strip-scores t))
  (mapcar (if strip-scores #'first #'identity)
          (ranked-low (lowest-chunk 0.1
                            (scored-with-average-tfidf tv-sentences)
                            (scored-with-length-deviation tv-sentences))
              :n n)))

(defun atypical (tv-sentences n &key (strip-scores t))
  (mapcar (if strip-scores #'first #'identity)
          (ranked-high (corrected-with #'*
                               (corrected-with #'/
                                               (scored-with-average-tfidf tv-sentences)
                                               (scored-with-length-deviation tv-sentences))
                               (scored-with-markers tv-sentences))
               :n n)))

(defun phrases-info (tv-sentences freq num-examples &key (sents-to-text t) (give-atypical nil))
  (let ((phrases (top-bigrams tv-sentences :freq freq)))
    (mapcar (lambda (phrase-entry)
              (let* ((phrase (car phrase-entry)) ; the cdr is frequency
                     (containing-sents (sentences-with-ngram phrase tv-sentences)))
                (append
                  (list :phrase phrase
                        :typical
                        (mapcar (if sents-to-text #'raw-text #'identity)
                                (typical containing-sents num-examples)))
                  (when give-atypical
                    (list :atypical (mapcar (if sents-to-text #'raw-text #'identity)
                                                   (atypical containing-sents num-examples)))))))
            ;; Sort - give the most frequent ones first.
            (sort (alexandria:hash-table-alist phrases)
                  #'> 
                  :key #'cdr))))
