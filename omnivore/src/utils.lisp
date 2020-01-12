(in-package :omnivore)

;; TODO iterate multi-array? (collocations)

(defun truep (arg) (not (not arg)))

(defun last-element (sequence)
  "Works for lists, recommended for vectors (use last-car instead)."
  (elt sequence (1- (length sequence))))

(defun last-car (list)
  (first (last list)))

(defun list-wrapped (object)
  (when object (list object)))

(defun penultimate-element (sequence)
  "Works for lists, recommended for vectors."
  (elt sequence (- (length sequence) 2)))

(defun list-longer-p (list n)
  "Return t if the list is longer than n."
  (let ((length-counter 0))
    (dolist (element list nil)
      (declare (ignore element))
      (incf length-counter)
      (when (> length-counter n)
        (return-from list-longer-p t)))))

(defun pulled-to-front (element-n list)
  "Return a copy of the list where the element under element-n is made the head of the list, with
   the rest unchanged."
  (append (subseq list element-n (1+ element-n))
          (subseq list 0 element-n)
          (subseq list (1+ element-n))))

;; from HyperSpec, DEFINE-MODIFY-MACRO
(define-modify-macro appendf (&rest args) append
  "Append onto list")

(defun n-replace-once (list element replacement &key (test #'equalp))
  (do ((next-cons list (cdr next-cons)))
      ((null next-cons) list)
    (when (funcall test (car next-cons) element)
      (setf (car next-cons) replacement)
      (return-from n-replace-once list))))
;;; (n-replace-once (list 25 24 26 45) 24 30)
;;; -> (25 30 26 45)

(defun csv-sanitized-string (string)
  (remove #\' (remove #\" string)))

(defun third-person-present (lemma)
  (cond ((>= 1 (length lemma)) lemma)
        ((equalp lemma "be") "is")
        ;; o, x, ss, sh, ch -> +es
        ((or (eq (last-element lemma) #\o)
             (eq (last-element lemma) #\x)
             (and (eq (penultimate-element lemma) #\s) ; ss
                  (eq (last-element lemma) #\s))
             (and (eq (last-element lemma) #\h) ; sh, ch
                  (or (eq (penultimate-element lemma) #\s)
                      (eq (penultimate-element lemma) #\c))))
         (concatenate 'string lemma "es"))
        ;; y -> ies
        ((and (eq (last-element lemma) #\y)
              (not (find (penultimate-element lemma)
                         "eoiua")))
         (concatenate 'string
                      ;; remove the last y, to replace with i in conjugation
                      (subseq lemma 0 (1- (length lemma)))
                      "ies"))
        ;; the default case
        (t (concatenate 'string lemma "s"))))

(defun xml-path (first-node &rest tag-names)
  "Starting from first-node, try to find a plump:element going through a path of tag-names through
   the XML tree."
  (do ((parent-node first-node)
       (child-tag-name (pop tag-names) (pop tag-names)))
      ((null child-tag-name) parent-node)
      (setf parent-node
            (or (find child-tag-name (plump:child-elements parent-node)
                      :key #'plump:tag-name :test #'equalp)
                (error (format nil "Cannot find tag ~A under the parent"
                               child-tag-name))))))

(defun gaussian (value mean standard-deviation)
  "Compute probability density function of normal distribution with the mean and standard-deviation."
  (/ (exp (* -0.5 (expt (/ (- value mean) ; exp is e^(...)
                           standard-deviation)
                        2))) ; (x-mu/sigma)^2
     (* standard-deviation (sqrt (* 2.0 pi)))))
