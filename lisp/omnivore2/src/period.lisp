(in-package :omnivore2)

(defun text-sentences (text &optional (spacy-doc nil))
  "Get the text sentences as a vector of strings. You can supply an existing spacy-doc for the \
text, A spacy doc is also returned as the second value."
  (let ((spacy-doc (or spacy-doc (spacy-doc-for text))))
    (values
     (string-sequence-from-spacy-obj
      (py4cl:python-eval spacy-doc ".sents")
      :type 'vector)
     spacy-doc)))

(defun text-char-ngrams (n text)
  "Return all string character n-grams as a sorted list of strings."
  (let ((chars (cl-strings:chars text))
        (ngrams))
    (dotimes (char-n (length chars))
      (when (< (+ char-n n) (length chars))
        (push (apply #'concatenate
                     (cons 'string (mapcar #'string (subseq chars char-n (+ char-n n)))))
              ngrams)))
    (sort ngrams #'string<)))

(defun common-string-count (list-1 list-2)
  (do ((elem-n-1 0)
       (elem-n-2 0)
       ;; We need to deduplicate, otherwise the result could be different if switching the argument
       ;; order.
       (list-1 (remove-duplicates list-1 :test #'equalp))
       (list-2 (remove-duplicates list-2 :test #'equalp))
       (common-count 0))
      ((or (= elem-n-1 (length list-1)) (= elem-n-2 (length list-2)))
       common-count)
    (cond ((equalp (elt list-1 elem-n-1) (elt list-2 elem-n-2))
           (incf common-count)
           (incf elem-n-2))
          ((string< (elt list-1 elem-n-1) (elt list-2 elem-n-2))
           (incf elem-n-1))
          ((string> (elt list-1 elem-n-1) (elt list-2 elem-n-2))
           (incf elem-n-2)))))

(defun periods-from-sentences (sentences)
  (let ((sentence-lengths (map 'vector (lambda (sentence) (length (cl-strings:split sentence)))
                               sentences))
        (sent-n 0)
        (periods)
        (period-lengths)
        (period-sentence-counts))
    (dotimes (sent-n (length sentences)
                     ;; Return periods themselves, their lengths in words and sents.
                     (values (reverse periods)
                             (reverse period-lengths)
                             (reverse period-sentence-counts)))
      (cond ((null periods)
             (push (elt sentences sent-n) periods)
             (push (elt sentence-lengths sent-n) period-lengths)
             (push 1 period-sentence-counts))
            ((or (< (car period-lengths) *good-minimal-period-length*) ; continue a too short period
                 (and (= (1+ sent-n) (length sentences)) ; join a short last sentence
                      (< (elt sentence-lengths sent-n) *good-minimal-period-length*)))
             (setf (car periods) (concatenate 'string (car periods) " " (elt sentences sent-n)))
             (incf (car period-lengths) (+ 1 (elt sentence-lengths sent-n)))
             (incf (car period-sentence-counts)))
            ;; Potentially join to the previous period if it looks similar enough.
            ((and (not (and (= (+ 2 sent-n) (length sentences)); don't join if the last sent is short
                            (< (elt sentence-lengths (1- (length sentences)))
                               *good-minimal-period-length*)))
                  (< (+ 1 (car period-lengths) (elt sentence-lengths sent-n))
                     *good-maximal-period-length*)
                  (>= (common-string-count (text-char-ngrams 3 (car periods))
                                           (text-char-ngrams 3 (elt sentences sent-n)))
                      *common-ngrams-count-for-merging*))
             (setf (car periods) (concatenate 'string (car periods) " " (elt sentences sent-n)))
             (incf (car period-lengths) (+ 1 (elt sentence-lengths sent-n)))
             (incf (car period-sentence-counts)))
            (t
             (push (elt sentences sent-n) periods)
             (push (elt sentence-lengths sent-n) period-lengths)
             (push 1 period-sentence-counts))))))

(defun text-field-name-for-language (language-code-string)
  (or (cdr (assoc language-code-string
                  *language-code->text-field-name* :test #'equalp))
      *other-languages-text-field-name*))

(defun period-text (period-alist)
  "The text of the textual period, retrieved from the appropriate field for its language."
  (cdr (assoc (text-field-name-for-language (cdr (assoc :language--code period-alist)))
              period-alist)))
