(in-package :omnivore)

(defun truep (arg) (not (not arg)))

;; from HyperSpec, DEFINE-MODIFY-MACRO
(define-modify-macro appendf (&rest args) append
  "Append onto list")
