(in-package :speechtractor)

(defun forums-doc-startp (node path)
  (declare (ignore path))
  (or
    ;; Xenforo
    (and (plump:has-attribute node "class")
         (cl-ppcre:scan (boundary-regex "message") (plump:attribute node "class")))
    ;; some old ver of phpBB
    (and (plump:has-attribute node "class")
         (cl-ppcre:scan (boundary-regex "thread_post") (plump:attribute node "class")))))
