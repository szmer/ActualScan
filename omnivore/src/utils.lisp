(in-package :omnivore)

(defun truep (arg) (not (not arg)))

(defun list-longer-p (list n)
  "Return t if the list is longer than n."
  (let ((length-counter 0))
    (dolist (element list nil)
      (declare (ignore element))
      (incf length-counter)
      (when (> length-counter n)
        (return-from list-longer-p t)))))

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

(defun third-person-present (lemma)
  (cond ((equalp lemma "be") "is")
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
