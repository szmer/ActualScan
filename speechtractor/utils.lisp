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

(defun boundary-regex (expr)
  "Due to \\b not functioning as we'd hoped, this generated a regex guaranteeing that the regex\
   can be only next to the string boundary or a whitespace."
  (format nil "(\\s~A\\s|^~A\\s|\\s~A$|^~A$)" expr expr expr expr))

(defun server-debug-print (string)
  (hunchentoot:acceptor-log-message speechtractor::*server* :info string))