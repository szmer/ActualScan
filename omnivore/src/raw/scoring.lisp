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
    (if n
        (subseq ranking 0 (min n (length ranking)))
        ranking)))

(defun ranked-low (scored-sentences &key (n nil))
  "Scored-sentences are provided in '(sent score) format."
  (let ((ranking (sort scored-sentences
                       #'<
                       :key #'second)))
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
        (deciding-sorted (sort scored-sentences #'> :key #'second)))
    (when *debug-scoring*
      (format t " Getting the highest chunk, taking ~A out of ~A (~A and above)~%"
              (floor (* proportion (length deciding-sorted)))
              (length deciding-sorted)
              (second (elt deciding-sorted (1- (floor (* proportion (length deciding-sorted))))))))
    ;; Create a hashed set of the first proportion of the best sentences.
    (dolist (sentence-entry (subseq deciding-sorted 0 (floor (* proportion (length deciding-sorted)))))
      (setf (gethash (raw-text (first sentence-entry)) included-sentences) t))
    (remove-if (lambda (sentence-entry)
                 (if (gethash (raw-text (first sentence-entry)) included-sentences)
                     nil
                     t))
               scored-sentences)))

(defun lowest-chunk (proportion scored-sentences deciding-sentences)
  "Return only the (proportion) of the lowest sentence entries from scored-sentences according to \
   deciding-sentences. Note that sentences are compared with their raw text."
  (let ((included-sentences (make-hash-table :test #'equalp))
        (deciding-sorted (sort scored-sentences #'< :key #'second)))
    (when *debug-scoring*
      (format t " Getting the lowest chunk, taking ~A out of ~A (~A and below)~%"
              (floor (* proportion (length deciding-sorted)))
              (length deciding-sorted)
              (second (elt deciding-sorted (1- (floor (* proportion (length deciding-sorted))))))))
    ;; Create a hashed set of the first proportion of the best sentences.
    (dolist (sentence-entry (subseq deciding-sorted 0 (floor (* proportion (length deciding-sorted)))))
      (setf (gethash (raw-text (first sentence-entry)) included-sentences) t))
    (remove-if (lambda (sentence-entry)
                 (if (gethash (raw-text (first sentence-entry)) included-sentences)
                     nil
                     t))
               scored-sentences)))

(defun scored-with-average-tfidf (sentences)
  "Return the sentences with assigned average tf-idf scores of their tokens. The list is suitable\
   for correcting and ranking functions."
  (let ((term-doc-freqs (make-hash-table :test #'equalp)))
    ;; Collect the doc frequency table info.
    (dolist (sentence sentences)
      (dolist (token (remove-duplicates (division-divisions sentence) :key #'division-raw-text
                                        :test #'equalp))
        (if (gethash (division-raw-text token) term-doc-freqs)
            (incf (gethash (division-raw-text token) term-doc-freqs))
            (setf (gethash (division-raw-text token) term-doc-freqs) 1))))
    ;; Compute its score for each sentence.
    (mapcar (lambda (sentence)
              (let ((score 0.0))
                (dolist (token (division-divisions sentence))
                  ;; add the idf to the score, multiplied by 1.
                  (incf score (/ (length sentences)
                                 (gethash (division-raw-text token) term-doc-freqs))))
                (list sentence
                      ;; compute the average tf-idf
                      (* score
                         (length (division-divisions sentence))))))
            sentences)))

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
