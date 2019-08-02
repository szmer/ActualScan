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
