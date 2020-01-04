(in-package :omnivore)

(defun preview-first (scored-sentences &key (n 10))
  (dotimes (sentence-entry-n n)
    (if (nth sentence-entry-n scored-sentences)
        (let ((sentence-entry (nth sentence-entry-n scored-sentences)))
          (format t "~A ~A~%" (raw-text (car sentence-entry)) (second sentence-entry)))
        (return-from preview-first))))

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

(defun corrected-with-length-deviation (scored-sentences update-fun)
  "Scored-sentences are provided in '(sent score) format. Return the list with scores updated \
   by update-fun with each sentence's deviation from the average length."
  (let ((length-average 0.0))
    (dolist (sentence-entry scored-sentences)
      (incf length-average (length (division-divisions (car sentence-entry)))))
    (setf length-average (/ length-average (length scored-sentences)))
    (mapcar (lambda (sentence-entry)
              (list (car sentence-entry)
                    (funcall update-fun
                             (second sentence-entry)
                             (let ((deviation (abs (- length-average
                                                      (length (division-divisions
                                                                (car sentence-entry)))))))
                               (if (zerop deviation)
                                   1e-20 ; avoid division by zero
                                   deviation)))))
            scored-sentences)))
