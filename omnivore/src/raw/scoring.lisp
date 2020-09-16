;;;;;
;;;;; Score sentences for business purposes.
;;;;;
(declaim (optimize (space 3)))

;;;; TODO BUG scoring the same list of sents two times messes up the older ranking?

(in-package :omnivore)

(defun preview-top (factor-name scored-sentences &key (n 10))
  (dotimes (sentence-entry-n n)
    (let ((sentence (nth sentence-entry-n scored-sentences))))
    (if (and sentence (read-attribute sentence factor-name))
        (format t "~A ~A~%" (raw-text sentence) (read-attribute sentence factor-name))
        (return-from preview-top))))

(defun ranked-high (factor-name scored-sentences &key (n nil))
  (let ((ranking (sort (remove-if (lambda (sentence)
                                               (not (read-attribute sentence factor-name)))
                                             scored-sentences)
                       #'>
                       :key (lambda (sentence) (read-attribute sentence factor-name)))))
    (when *debug-scoring*
      (format t "Taking high ranked - received ~A sents~%" (length ranking)))
    (if n
        (subseq ranking 0 (min n (length ranking)))
        ranking)))

(defun ranked-low (factor-name scored-sentences &key (n nil))
  (let ((ranking (sort (remove-if (lambda (sentence)
                                               (not (read-attribute sentence factor-name)))
                                             scored-sentences)
                       #'<
                       :key (lambda (sentence) (read-attribute sentence factor-name)))))
    (when *debug-scoring*
      (format t "Taking low ranked - received ~A sents~%" (length ranking)))
    (if n
        (subseq ranking 0 (min n (length ranking)))
        ranking)))

(defun highest-chunk (proportion deciding-factor-name scored-sentences)
  "Return only the (proportion) of the highest sentences from scored-sentences according to \
   deciding-factor-name."
  (let ((included-sentences (make-hash-table :test #'equalp))
        (sorted-by-deciding (sort (remove-if (lambda (sentence)
                                               (not (read-attribute sentence deciding-factor-name)))
                                             scored-sentences)
                                  #'>
                                  :key (lambda (sentence) (read-attribute sentence
                                                                          deciding-factor-name)))))
    (when *debug-scoring*
      (format t " Getting the highest chunk, taking ~A out of ~A (~A and above)~%"
             (length (subseq scored-sentences 0 (floor (* proportion (length scored-sentences)))))
              (length scored-sentences)
             (read-attribute (elt sorted-by-deciding
                                  (1- (floor (* proportion (length sorted-by-deciding)))))
                             deciding-factor-name)))
    ;; Create a hashed set of the first proportion of the best sentences.
    (dolist (sentence (subseq sorted-by-deciding 0
                              (floor (* proportion (length sorted-by-deciding)))))
      (setf (gethash sentence included-sentences) t))
    (remove-if (lambda (sentence)
                 (if (gethash sentence included-sentences) nil t))
               scored-sentences)))

(defun lowest-chunk (proportion deciding-factor-name scored-sentences)
  "Return only the (proportion) of the lowest sentences from scored-sentences according to \
   deciding-factor-name."
  (let ((included-sentences (make-hash-table :test #'equalp))
        (sorted-by-deciding (sort (remove-if (lambda (sentence)
                                               (not (read-attribute sentence deciding-factor-name)))
                                             scored-sentences)
                                  #'<
                                  :key (lambda (sentence) (read-attribute sentence
                                                                          deciding-factor-name)))))
    (when *debug-scoring*
      (format t " Getting the lowest chunk, taking ~A out of ~A (~A and below)~%"
             (length (subseq scored-sentences 0 (floor (* proportion (length sorted-by-deciding)))))
             (length sorted-by-deciding)
             (read-attribute (elt sorted-by-deciding
                                  (1- (floor (* proportion (length sorted-by-deciding)))))
                             deciding-factor-name)))
    ;; Create a hashed set of the first proportion of the best sentences.
    (dolist (sentence (subseq sorted-by-deciding 0
                              (floor (* proportion (length sorted-by-deciding)))))
      (setf (gethash sentence included-sentences) t))
    (remove-if (lambda (sentence)
                 (if (gethash sentence included-sentences) nil t))
               scored-sentences)))

(defun add-corrected-attribute (new-factor-name update-fun scored-sentences factor-name
                                                correcting-factor-name
                                                &key (magnitude 1.0) (overwrite nil))
  (when (or (and (first scored-sentences) (not (read-attribute (first scored-sentences)
                                                               new-factor-name)))
            overwrite)
    (dolist (sentence scored-sentences)
      (setf (gethash new-factor-name (record-meta sentence))
            (funcall update-fun
                     (read-attribute sentence factor-name)
                     (if (or (not (read-attribute sentence correcting-factor-name))
                             (zerop (read-attribute sentence correcting-factor-name)))
                         1e-20 ; avoid division by zero
                         (* magnitude
                            (read-attribute sentence correcting-factor-name)))))))
  scored-sentences)

(defun scored-with-average-tfidf! (sentences &key (overwrite nil))
  "Return the sentences with assigned average tf-idf scores of their tokens. The list is suitable\
   for correcting and ranking functions."
  (when (or (and (first sentences) (not (read-attribute (first sentences) "average_tf_idf")))
            overwrite)
   (let ((term-doc-freqs (make-hash-table :test #'equalp)))
    (mapc (lambda (sentence)
              (let ((score-sum 0.0))
                (maphash (lambda (term tf-idf) (incf score-sum tf-idf))
                         (read-attribute sentence "tf_idf_table"))
                (setf (gethash "average_tf_idf" (record-meta sentence))
                      (/ score-sum (length (division-divisions sentence))))))
            (sentences-with-tf-idfs! sentences))))
  sentences)

(defun scored-with-length-deviation! (sentences &key (overwrite nil))
  "Score sentences with their absolute deviation of length from the average."
  (when (or (and (first sentences) (not (read-attribute (first sentences) "length_deviation")))
            overwrite)
    (let ((length-average 0.0))
      (dolist (sentence sentences)
        (incf length-average (length (division-divisions sentence))))
      (setf length-average (/ length-average (length sentences)))
      (when *debug-scoring* (format t " The average sentence length is ~A~%" length-average))
      (dolist (sentence sentences)
        (setf (gethash "length_deviation" (record-meta sentence))
              (abs (- length-average (length (division-divisions sentence))))))))
  sentences)

;;;-(defun scored-with-markers (sentences &optional (markers '("USAGE")))
;;;-  (mapcar (lambda (sentence)
;;;-            (list sentence
;;;-                  (apply
;;;-                    #'max ; of all the markers searched for
;;;-                    (mapcar
;;;-                      (lambda (marker)
;;;-                              (marker-presence
;;;-                                marker
;;;-                                ;; Token texts (TODO should be lemmas)
;;;-                                (mapcar #'division-raw-text (division-divisions sentence))))
;;;-                                 markers))))
;;;-          sentences))

(defun scored-with-age! (tv-divisions &key (dummy-value 1e15) (overwrite nil))
  (when (or (and (first tv-divisions) (not (read-attribute (first tv-divisions) "age")))
            overwrite)
    (dolist (division tv-divisions)
      (setf (gethash "age" (record-meta division))
            (if (not (read-attribute division "publication-date"))
                dummy-value
                (local-time:timestamp-to-universal
                  (local-time:parse-rfc3339-timestring
                    (read-attribute division "publication-date")))))))
  tv-divisions)

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
