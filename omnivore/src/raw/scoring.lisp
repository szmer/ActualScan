(in-package :omnivore)

(defun preview-top (scored-sentences &key (n 10))
  (dotimes (sentence-entry-n n)
    (if (nth sentence-entry-n scored-sentences)
        (let ((sentence-entry (nth sentence-entry-n scored-sentences)))
          (format t "~A ~A~%" (raw-text (car sentence-entry)) (second sentence-entry)))
        (return-from preview-top))))

(defun in-length-range (range scored-sentences)
  "Range should be provided as '(min max+1)"
  (remove-if (lambda (sentence-entry)
               (or (> (car range) (length (division-divisions (car sentence-entry))))
                   (<= (second range) (length (division-divisions (car sentence-entry))))))
             scored-sentences))

(defun ranked-best (scored-sentences)
  "Scored-sentences are provided in '(sent score) format."
  (sort scored-sentences
        #'>
        :key #'second))

(defun ranked-worst (scored-sentences)
  "Scored-sentences are provided in '(sent score) format."
  (sort scored-sentences
        #'< ;; the lowest score first
        :key #'second))

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
                    #'max
                    (mapcar
                      (lambda (marker)
                              (marker-presence
                                marker
                                ;; Token texts (TODO should be lemmas)
                                (mapcar #'division-raw-text (division-divisions sentence))))
                                 markers))))
          sentences))
