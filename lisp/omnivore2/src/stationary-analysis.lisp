(in-package :omnivore2)

(defun add-average-word-length-for-period (period-alist &optional (spacy-doc nil))
  "You can supply an existing spacy-doc for the period. A spacy doc will be also returned as a \
second value."
  (let* ((spacy-doc (or spacy-doc
                        (spacy-doc-for (cdr (assoc :text period-alist)))))
         (word-lengths (mapcar #'length (string-sequence-from-spacy-obj spacy-doc))))
    (push (cons :average--word--length (/ (reduce #'+ word-lengths)
                                 (length word-lengths)))
          period-alist)
    (values
     period-alist
     spacy-doc)))

(defun period-entries-from-alist (text-alist)
  "Given an alist containing :text and possibly other keys, produce a list of alists for each \
text period extracted. Add the entries for character, sentence length, the length of the document \
in periods and consecutive period numbers."
  (multiple-value-bind (periods char-lengths sent-lengths)
      (periods-from-sentences (text-sentences (cdr (assoc :text text-alist))))
    (mapcar (lambda (period period-n char-length sent-length)
              (let ((period-alist (copy-alist text-alist)))
                (setf (cdr (assoc :text period-alist))
                      period)
                (push (cons :word--length char-length)
                      period-alist)
                (push (cons :sent--length sent-length)
                      period-alist)
                (push (cons :parent--document--length (length periods))
                      period-alist)
                (push (cons :period--number period-n)
                      period-alist)
                period-alist))
            periods (alexandria:iota (length periods) :start 1) char-lengths sent-lengths)))

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
