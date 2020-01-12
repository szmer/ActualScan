(in-package :omnivore)

(defun sentences-with-ngram (n-gram tv-sentences)
  "The n-gram should be supplied as a list."
  (let ((result))
    (dolist (sent tv-sentences)
      (when (search n-gram (mapcar #'division-raw-text (division-divisions sent)) :test #'equalp)
        (push sent result)))
    result))

(defun make-cumulator (&key (remember-size 800) (keep-freq 5) (comparison-function #'equalp))
  "Make a cumulator focusing on finding items more frequent than keep-freq, Increasing remmber-size\
   improves recall and increases memory footprint. Two functions are return: the register function\
   to call on all elements to be counted, and a flush function to get the result as a hash table."
  (assert (> remember-size 100))
  (let ((low-freqs (make-array (list (1- keep-freq) (round (/ remember-size 100)))))
        (filled-table-ns (make-array (list (1- keep-freq)) :initial-element 0))
        (kept-freqs (make-hash-table :test comparison-function)))
    ;; We need to fill the low-freqs array with separate fresh hash tables.
    (dotimes (freq (array-dimension low-freqs 0))
      (dotimes (table-n (array-dimension low-freqs 1))
        (setf (aref low-freqs freq table-n) (make-hash-table :test #'equalp))))
    (values
      ;;
      ;; Register element function.
      ;; NOTE freqs are zero-base here.
      (labels ((%place-in-freq (freq element)
                 ;; Remove the next filled older table if needed.
                 (when (= remember-size (hash-table-count
                                          (aref low-freqs freq (aref filled-table-ns freq))))
                   ;; Set the new filled table for this freq.
                   (if (> (array-dimension low-freqs 1) (1+ (aref filled-table-ns freq)))
                       (incf (aref filled-table-ns freq))
                       (setf (aref filled-table-ns freq) 0))
                   ;; Clear the table under the new pointer.
                   (clrhash (aref low-freqs freq (aref filled-table-ns freq))))
                 ;; Actually make the entry.
                 (setf (gethash element (aref low-freqs freq (aref filled-table-ns freq))) t)))
        (lambda (element)
          (if (gethash element kept-freqs)
            (incf (gethash element kept-freqs))
            (or 
              (block find-table
                     ;; Try to find if the element is somewhere among low-freqs tables.
                     (dotimes (freq (array-dimension low-freqs 0))
                       (dotimes (table-n (array-dimension low-freqs 1))
                         (when (gethash element (aref low-freqs freq table-n))
                           (remhash element (aref low-freqs freq table-n))
                           ;; (we need to add 2 here, because freq index is less by one than the
                           ;; frequency itself)
                           (if (= (+ 2 freq) keep-freq)
                               (setf (gethash element kept-freqs) keep-freq)
                               (%place-in-freq (1+ freq) element))
                           (return-from find-table t)))))
              ;; If not found, place among hapaxes.
              (%place-in-freq 0 element)))))
      ;;
      ;; Get contents function.
      (lambda (&key (include-lowfreq nil))
        ;; Add the info from low-freqs to the kept-freqs table and return it.
        (when include-lowfreq
          (dotimes (freq (array-dimension low-freqs 0))
            (dotimes (table-n (array-dimension low-freqs 1))
              (maphash (lambda (element _)
                         (setf (gethash element kept-freqs) (1+ freq)))
                       (aref low-freqs freq table-n)))))
        kept-freqs))))

(defun test-make-cumulator ()
  ;; TODO also test case when lowfreq tables need to be swapped and cleared.
  (let ((text (list 23 26 12 432 23 23 23 23 23)))
    (multiple-value-bind (register flush)
      (make-cumulator)
      (dolist (item text) (funcall register item))
      (let ((table (funcall flush :include-lowfreq t)))
        (assert (eq 6 (gethash 23 table))) 
        (assert (eq 1 (gethash 12 table)))))
    t))

(defun top-bigrams (tv-sentences &key (freq 10))
  (multiple-value-bind (register flush)
    (make-cumulator :keep-freq freq)
    (dolist (sent tv-sentences)
      (do ((second-token-n 1 (1+ second-token-n)))
          ((>= second-token-n (length (division-divisions sent))))
          (let ((word1 (string-downcase
                                   (division-raw-text
                                 (elt (division-divisions sent) (1- second-token-n)))))
                (word2 (string-downcase
                                   (division-raw-text
                                     (elt (division-divisions sent) second-token-n)))))
            
            (unless (or (stopwordp word1) (stopwordp word2))
            (funcall register (list word1 word2))))))
    (funcall flush)))

(defun top-tree-linkages (tv-sentences &key (freq 10))
  (multiple-value-bind (register flush)
    (make-cumulator :keep-freq freq)
    (dolist (sent tv-sentences)
      (let ((tree (sentence-tree sent)))
        (dolist (token (cl-conllu:sentence-tokens tree))
          (unless (or (equalp (cl-conllu:token-deprel token) "ROOT")
                      (stopwordp (string-downcase (cl-conllu:token-lemma token)))
                      (stopwordp (string-downcase (cl-conllu:token-lemma
                                                    (elt (cl-conllu:sentence-tokens tree)
                                                         (1- (cl-conllu:token-head token)))))))
            (funcall register (list (string-downcase (cl-conllu:token-lemma token))
                                      (string-downcase (cl-conllu:token-lemma
                                                         (elt (cl-conllu:sentence-tokens tree)
                                                              (1- (cl-conllu:token-head token)))))))))))
    (funcall flush)))
