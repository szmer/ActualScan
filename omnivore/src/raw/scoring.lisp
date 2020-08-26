;;;;;
;;;;; Score sentences for business purposes.
;;;;;

;;;; TODO BUG scoring the same list of sents two times messes up the older ranking?

(in-package :omnivore)

(defun preview-top (scored-sentences &key (n 10))
  (dotimes (sentence-entry-n n)
    (if (nth sentence-entry-n scored-sentences)
        (let ((sentence-entry (nth sentence-entry-n scored-sentences)))
          (format t "~A ~A~%" (raw-text (car sentence-entry)) (second sentence-entry)))
        (return-from preview-top))))

(defun ranked-high (scored-sentences &key (n nil))
  "Scored-sentences are provided in '(sent score) format."
  (let ((ranking (sort scored-sentences
                       #'>
                       :key #'second)))
    (when *debug-scoring*
      (format t "Taking high ranked - received ~A sents~%" (length ranking)))
    (if n
        (subseq ranking 0 (min n (length ranking)))
        ranking)))

(defun ranked-low (scored-sentences &key (n nil))
  "Scored-sentences are provided in '(sent score) format."
  (let ((ranking (sort scored-sentences
                       #'<
                       :key #'second)))
    (when *debug-scoring*
      (format t "Taking low ranked - received ~A sents~%" (length ranking)))
    (if n
        (subseq ranking 0 (min n (length ranking)))
        ranking)))

(defun corrected-with (update-fun scored-sentences correcting-scored-sentences &key (magnitude 1.0))
  "Scored-sentences and corrections are provided in '(sent score) format. Return the list with \
   scores updated by update-fun with corrections."
  (mapcar
    (lambda (sentence-entry correcting-entry)
      (list (car sentence-entry)
            (funcall update-fun
                     (second sentence-entry)
                     (if (zerop (second correcting-entry))
                         1e-20 ; avoid division by zero
                         (* magnitude
                            (second correcting-entry))))))
    scored-sentences correcting-scored-sentences))

(defun highest-chunk (proportion scored-sentences deciding-sentences)
  "Return only the (proportion) of the highest sentence entries from scored-sentences according to \
   deciding-sentences. Note that sentences are compared with their raw text."
  (let ((included-sentences (make-hash-table :test #'equalp))
        (deciding-sorted (sort deciding-sentences #'> :key #'second)))
    (when *debug-scoring*
      (format t " Getting the highest chunk, taking ~A out of ~A (~A and above)~%"
              (length (subseq deciding-sorted 0 (floor (* proportion (length deciding-sorted)))))
              (length deciding-sorted)
              (second (elt deciding-sorted (1- (floor (* proportion (length deciding-sorted))))))))
    ;; Create a hashed set of the first proportion of the best sentences.
    (dolist (sentence-entry (subseq deciding-sorted 0 (floor (* proportion (length deciding-sorted)))))
      (setf (gethash (first sentence-entry) included-sentences) t))
    (remove-if (lambda (sentence-entry)
                 (if (gethash (first sentence-entry) included-sentences)
                     nil
                     t))
               scored-sentences)))

(defun lowest-chunk (proportion scored-sentences deciding-sentences)
  "Return only the (proportion) of the lowest sentence entries from scored-sentences according to \
   deciding-sentences. Note that sentences are compared with their raw text."
  (let ((included-sentences (make-hash-table :test #'equalp))
        (deciding-sorted (sort deciding-sentences #'< :key #'second)))
    (when *debug-scoring*
      (format t " Getting the lowest chunk, taking ~A out of ~A (~A and below)~%"
              (length (subseq deciding-sorted 0 (floor (* proportion (length deciding-sorted)))))
              (length deciding-sorted)
              (second (elt deciding-sorted (1- (floor (* proportion (length deciding-sorted))))))))
    ;; Create a hashed set of the first proportion of the best sentences.
    (dolist (sentence-entry (subseq deciding-sorted 0 (floor (* proportion (length deciding-sorted)))))
      (setf (gethash (first sentence-entry) included-sentences) t))
    (remove-if (lambda (sentence-entry)
                 (if (gethash (first sentence-entry) included-sentences)
                     nil
                     t))
               scored-sentences)))

(defun scored-with-average-tfidf (sentences)
  "Return the sentences with assigned average tf-idf scores of their tokens. The list is suitable\
   for correcting and ranking functions."
  (let ((term-doc-freqs (make-hash-table :test #'equalp)))
    (mapcar (lambda (sentence sent-tf-idfs)
              (let ((score-sum 0.0))
                (maphash (lambda (term tf-idf) (incf score-sum tf-idf))
                         sent-tf-idfs)
                (list sentence
                      (/ score-sum (length (division-divisions sentence))))))
            sentences
            (tf-idfs-for-sentences sentences))))

(defun scored-with-length-deviation (sentences)
  "Score sentences with their absolute deviation of length from the average."
  (let ((length-average 0.0))
    (dolist (sentence sentences)
      (incf length-average (length (division-divisions sentence))))
    (setf length-average (/ length-average (length sentences)))
    (when *debug-scoring*
      (format t " The average sentence length is ~A~%" length-average))
    (mapcar (lambda (sentence)
              (list sentence
                    (let ((deviation (abs (- length-average
                                             (length (division-divisions sentence))))))
                      deviation)))
            sentences)))

(defun scored-with-markers (sentences &optional (markers '("USAGE")))
  (mapcar (lambda (sentence)
            (list sentence
                  (apply
                    #'max ; of all the markers searched for
                    (mapcar
                      (lambda (marker)
                              (marker-presence
                                marker
                                ;; Token texts (TODO should be lemmas)
                                (mapcar #'division-raw-text (division-divisions sentence))))
                                 markers))))
          sentences))

(defun scored-with-age (tv-divisions &key (dummy-value 1e15))
  (mapcar
    (lambda (division)
      (list division
            (if (not (read-attribute division "publication-date"))
                           dummy-value
                           (local-time:timestamp-to-universal
                             (local-time:parse-rfc3339-timestring
                               (read-attribute division "publication-date"))))))
    tv-divisions))

(defun top-terms-by-tf-idfs (tf-idfs-list &key (first-n-scored 5) (cutoff 2) (remove-stopwords t))
  "Extract top terms as a list of two-elems lists from a list of tf-idfs hash tables for some docs.\
   Note this doesn't seem to work well, since the top terms are spread thinly - maybe with better\
   preprocessing?"
  (let ((term-scores (make-hash-table :test #'equalp))
        (term-score-values nil))
    (dolist (tf-idfs tf-idfs-list)
      (let ((term-score-values (alexandria:hash-table-alist tf-idfs)))
        (setf term-score-values (sort term-score-values #'> :key #'cdr))
        (dolist (entry (subseq term-score-values 0 (min first-n-scored (length term-score-values))))
          (if (gethash (division-raw-text (car entry)) term-scores)
              (incf (gethash (division-raw-text (car entry)) term-scores))
              (setf (gethash (division-raw-text (car entry)) term-scores) 1)))))
    (setf term-score-values (alexandria:hash-table-alist term-scores))
    (when remove-stopwords
      (setf term-score-values (remove-if (lambda (entry) (stopwordp (string-downcase (car entry))))
                                         term-score-values)))
    (setf term-score-values (sort term-score-values #'> :key #'cdr))
    (subseq term-score-values 0
            (or (position-if (lambda (entry) (> cutoff (cdr entry))) term-score-values)
                (length term-score-values)))))
