(in-package :omnivore)

(defun sentences-with-tf-idfs! (sentences &key (overwrite nil))
  "Add a tf-idf score hash table for tokens to each sentence. Unless the overwrite flag is set, the\
   old information will be left as is if found."
  (when (or (and (first sentences) (not (read-attribute (first sentences) "tf_idf_table")))
            overwrite)
    (let ((term-doc-freqs (make-hash-table :test #'equalp))) ; in how many docs/sents the term appears
    ;; Collect the doc frequency table info in term-doc-freqs.
    (dolist (sentence sentences)
      (dolist (token (remove-duplicates (division-divisions sentence) :key #'division-raw-text
                                        :test #'equalp)) ; skip duplicates
        (if (gethash (division-raw-text token) term-doc-freqs)
            (incf (gethash (division-raw-text token) term-doc-freqs))
            (setf (gethash (division-raw-text token) term-doc-freqs) 1))))
    ;; Compute its score for each sentence.
    (timed-execution
      (dolist (sentence sentences)
      (let ((sent-tf-idf-table (make-hash-table :test #'equalp)))
        (dolist (token (division-divisions sentence))
          (if (gethash token sent-tf-idf-table)
              ;; We add tf (1) / df (known) to the score
              (incf (gethash token sent-tf-idf-table)
                    (/ 1 (gethash (division-raw-text token) term-doc-freqs)))
              (setf (gethash token sent-tf-idf-table)
                    (/ 1 (gethash (division-raw-text token) term-doc-freqs)))))
        (setf (gethash "tf_idf_table" (record-meta sentence)) sent-tf-idf-table))))))
  sentences)

(defun tf-idfs-for-sections (sections)
  (let ((term-doc-freqs (make-hash-table :test #'equalp))) ; in how many docs the term appears
    ;; Collect the doc frequency table info in term-doc-freqs.
    (dolist (section sections)
      (dolist (token (remove-duplicates (reduce #'append
                                                (mapcar #'division-divisions
                                                        (division-divisions section)))
                                        :key #'division-raw-text
                                        :test #'equalp))
        (if (gethash (division-raw-text token) term-doc-freqs)
            (incf (gethash (division-raw-text token) term-doc-freqs))
            (setf (gethash (division-raw-text token) term-doc-freqs) 1))))
    ;; Compute its score for each section.
    (mapcar (lambda (section)
              (let ((sec-tf-idf-table (make-hash-table :test #'equalp)))
                (dolist (sentence (division-divisions section))
                  (dolist (token (division-divisions sentence))
                    (if (gethash token sec-tf-idf-table)
                        (incf (gethash token sec-tf-idf-table)
                              ;; add the idf to the score, multiplied by 1 (as the known df)
                              (/ (length sections)
                                 (gethash (division-raw-text token) term-doc-freqs)))
                        (setf (gethash token sec-tf-idf-table)
                              (/ (length sections)
                                 (gethash (division-raw-text token) term-doc-freqs))))))
                sec-tf-idf-table))
            sections)))

(defun tokens-frequency-list (tokens &key (cutoff 3) (remove-stopwords t))
  "Get a sorted a list of the most frequent terms among the tokens. The cutoff doesn't need to be \
   integer."
  (let ((term-frequencies (make-hash-table :test #'equalp))
        (term-frequency-list))
    (dolist (token tokens)
      (if (gethash (division-raw-text token) term-frequencies)
        (incf (gethash (division-raw-text token) term-frequencies))
        (setf (gethash (division-raw-text token) term-frequencies) 1)))
    (setf term-frequency-list (alexandria:hash-table-alist term-frequencies))
    (when remove-stopwords
      (setf term-frequency-list (remove-if (lambda (entry) (stopwordp (string-downcase (car entry))))
                                         term-frequency-list)))
    (setf term-frequency-list (sort term-frequency-list #'> :key #'cdr))
    (subseq term-frequency-list 0
            (or (position-if (lambda (entry) (> cutoff (cdr entry))) term-frequency-list)
                (length term-frequency-list)))))
