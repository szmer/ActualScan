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
