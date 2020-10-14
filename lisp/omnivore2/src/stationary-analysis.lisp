(in-package :omnivore2)

(defun add-average-word-length-for-period (period-alist &optional (spacy-doc nil))
  "You can supply an existing spacy-doc for the period. A spacy doc will be also returned as a \
second value."
  (let* ((spacy-doc (or spacy-doc
                        (spacy-doc-for (period-text period-alist))))
         (word-lengths (mapcar #'length (string-sequence-from-spacy-obj spacy-doc))))
    (push (cons :average--word--length--i (/ (reduce #'+ word-lengths)
                                 (length word-lengths)))
          period-alist)
    (values period-alist spacy-doc)))

(defun period-entries-from-alist (text-alist)
  "Given an alist containing :text and possibly other keys, produce a list of alists for each \
text period extracted. Add the entries for character, sentence length, the length of the document \
in periods and consecutive period numbers."
  (multiple-value-bind (sentences sent-spacy-items)
      (text-sentences (cdr (assoc :text text-alist)))
    (setf sent-spacy-items
          (py4cl:python-call "list" (py4cl:python-eval sent-spacy-items ".sents")))
    ;; Split the sentences into sequences in one language.
    (labels
        ((%language-for-sent-element (sent-element) (py4cl:python-eval sent-element
                                                                       "._.language['language']")))
      (let ((sentence-language-groups ; the first element is always the language code
              (do* ((sent-n 0 (1+ sent-n))
                    (sentence (ignore-errors (elt sentences sent-n))
                              (ignore-errors (elt sentences sent-n)))
                    (current-group)
                    (groups))
                   ((= sent-n (length sentences))
                    (reverse (if current-group (cons (reverse current-group) groups) groups)))
                (if (not current-group)
                    (progn (push (%language-for-sent-element (elt sent-spacy-items sent-n))
                                 current-group)
                           (push sentence current-group))
                    (if (equalp (%language-for-sent-element (elt sent-spacy-items sent-n))
                                (%language-for-sent-element (elt sent-spacy-items (1- sent-n))))
                        (push sentence current-group)
                        (progn (push current-group groups)
                               (setf current-group (list sentence
                                                         (%language-for-sent-element
                                                          (elt sent-spacy-items sent-n))))))))))
        (reduce #'append
                (mapcar (lambda (sentence-group)
                          (multiple-value-bind (periods char-lengths sent-lengths)
                              ;; (the first element of the group is the language code)
                              (periods-from-sentences (cdr sentence-group))
                          (mapcar (lambda (period period-n char-length sent-length)
                                    (let ((period-alist (copy-alist text-alist)))
                                      (push (cons :word--length--i char-length)
                                            period-alist)
                                      (push (cons :sent--length--i sent-length)
                                            period-alist)
                                      (push (cons :parent--document--length--i (length periods))
                                            period-alist)
                                      (push (cons :period--number--i period-n)
                                            period-alist)
                                      ;; Add the unique identification (Solr doc_location field,
                                      ;; url tab period number)
                                      (push (cons :doc--location
                                                  (format nil "~A~A~A"
                                                          (cdr (assoc :url period-alist))
                                                          #\Tab period-n))
                                            period-alist)
                                      ;; Leave only the text of period and move it to the
                                      ;; language-specific field.
                                      (setf (cdr (assoc :text period-alist)) period)
                                      (setf (car (assoc :text period-alist)) ; (change the key)
                                            (text-field-name-for-language (car sentence-group)))
                                      (push (cons :language--code (car sentence-group))
                                            period-alist)
                                      period-alist))
                                  periods
                                  (alexandria:iota (length periods) :start 1)
                                  char-lengths sent-lengths)))
                        sentence-language-groups))))))

(defun stationary-analysis-applied (text-alist)
  "Given an alist containing :text and possibly other keys, produce a list of alists for each \
text period extracted, and added stationary features that we can compute."
  (let ((period-alists (period-entries-from-alist text-alist)))
    (mapcar (lambda (period-alist)
              (if *stationary-analytic-funs*
                (multiple-value-bind (new-period-alist spacy-doc)
                    ;; We have to use eval to extract the function from its symbol.
                    (funcall (eval (first *stationary-analytic-funs*)) period-alist)
                  ;; Re-use the spacy-doc on the rest of the analytic functions
                  (dolist (fun-symbol (rest *stationary-analytic-funs*) new-period-alist)
                    (setf new-period-alist
                          (funcall (eval fun-symbol) new-period-alist spacy-doc))))
                ;; (If there is no functions, just pass thru the original alist)
                period-alist))
      period-alists)))
