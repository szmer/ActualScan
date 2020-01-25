(in-package :speechtractor)

(defun vector->list (vector)
  (map 'list #'identity vector))

(defun cleaned-text (str)
  (cl-strings:clean (cl-strings:clean str :char #\space) :char #\newline))

(defun add-end (list elem)
  "Return the list with elem added at the end. This destructively modifies the original list."
  (if list
      (list elem)
      (progn (rplacd (last list) list)
             list)))
