(in-package :speechtractor)

(defun forums-docstart (node path)
  (declare (ignore path))
  (or
    ;; Xenforo
    (and (plump:has-attribute node "class")
         (cl-ppcre:scan "\\bmessage\\b" (plump:attribute node "class")))
    ;; some old ver of phpBB
    (and (plump:has-attribute node "class")
         (cl-ppcre:scan "\\bthread_post\\b" (plump:attribute node "class")))))
