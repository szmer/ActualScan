(in-package :speechtractor)

(defun vector->list (vector)
  (map 'list #'identity vector))

(defun cleaned-text (str)
  (cl-strings:clean (cl-strings:clean str :char #\space) :char #\newline))

(defmacro add-end (list elem)
  "Return the list with elem (non-nil) added at the end. This destructively modifies the original \
   list."
  `(if ,list
      (progn (rplacd (last ,list) (list ,elem))
             ,list)
      (setf ,list (list ,elem))))
