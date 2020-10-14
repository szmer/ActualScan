(in-package :omnivore2)

(defun add-average-tf-idf-for-period (period-alist context-strings docs-map)
  "Add a :average--word--tf--idf entry to the period-alist, containing the average tf-idf score \
for a word taken from the period."
  (let ((term-doc-freq-table (make-hash-table :test #'equalp))
        (period-tokens (string-sequence-from-spacy-obj
                        (cdr (assoc :tmp-spacy-doc
                                    (gethash (period-text period-alist) docs-map)))))
        (accumulated-tf-idf-for-period 0.0))
    ;; Collect the document frequencies for terms.
    (dolist (context-str context-strings)
      (dolist (token (remove-duplicates
                      (string-sequence-from-spacy-obj
                       (cdr (assoc :tmp-spacy-doc (gethash context-str docs-map))))
                      :test #'equalp))
        (if (gethash token term-doc-freq-table)
            (setf (gethash token term-doc-freq-table) 1)
            (incf (gethash token term-doc-freq-table)))))
    (dolist (token period-tokens)
      (incf accumulated-tf-idf-for-period
            ;; the known df (1) multiplied by idf
            (/ 1 (gethash oken term-doc-freq-tablen))))
    ;; Add the main information.
    (push (assoc :average--word--tf--idf--i
                 (/ accumulated-tf-idf-for-period
                    (length period-tokens)))
          period-alist)))

(defun add-sentences-length-deviation-for-period (period-alist context-strings docs-map)
  "Add a :sentences--length--deviation entry to the period-alist, containing the average deviation \
of the length of a sentence (in words) from a period, when compared to the context. The number can \
be positive or negative depending on the direction of the deviation."
  (let ((accumulated-sentence-length 0.0)
        (context-sentence-count 0)
        (period-sentences (py4cl:python-eval
                           (cdr (assoc :tmp-spacy-doc
                                       (gethash (period-text period-alist) docs-map)))
                           ".sents")))
    ;; Collect information on sentences from the context.
    (dolist (context-str context-strings)
      (let ((context-sentences
              (py4cl:python-eval (cdr (assoc :tmp-spacy-doc (gethash context-str docs-map)))
                                 ".sents")))
        (incf context-sentence-count (py4cl:python-call "len" context-sentences))
        (dolist (sentence context-sentences)
          (incf accumulated-sentence-length
                (py4cl:python-call "len" sentence)))))
    ;; Add the main information.
    (let ((average-sentence-length (/ accumulated-sentence-length context-sentence-count)))
      (push
       (assoc :sentences--length--deviation--i
              (/
               (reduce #'+
                       (mapcar (lambda (sentence)
                                 (- (py4cl:python-call "len" sentence) average-sentence-length))
                               period-sentences))
               (py4cl:python-call "len" period-sentences)))
       period-alist))))

(defun apply-contextual-analysis (period-alist context-strings docs-map)
  "Fill an alist for a text period with fields from contextual analysis, using a list of strings \
representing the context docs retrieved from Solr. We will use (and extend as needed) the \
docs-map, which is a hash map of (text of period) -> (its alist of information, containing the \
information from Solr, the spacy doc and possibly other auxillary data written by analytic \
functions)."
  ;; Ensure that all the functions can expect as spacy document in the context alist.
  (dolist (context-str context-strings)
    (unless (assoc :tmp-spacy-doc (gethash context-str docs-map))
      (push (cons :tmp-spacy-doc (spacy-doc-for context-str))
            (gethash context-str docs-map))))
  ;; Do the same for the text of the period in question.
  (unless (assoc :tmp-spacy-doc (gethash (period-text period-alist) docs-map))
    (push (cons :tmp-spacy-doc (spacy-doc-for (period-text period-alist)))
          (gethash (period-text period-alist) docs-map)))
  (dolist (fun-symbol *contextual-analytic-funs* period-alist)
    (push (funcall (eval fun-symbol) period-alist context-strings docs-map)
          period-alist)))

(defun clean-period-alist (period-alist)
  "Remove duplicate and temporary entries from the period alist and return it."
  (let ((result-alist))
    (dolist (entry period-alist result-alist)
      (when (and (not (cl-strings:starts-with (format nil "~A" (car entry))
                                              ":TMP-"))
                 (not (assoc (car entry) result-alist)))
        (push entry result-alist)))))
