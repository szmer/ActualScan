(in-package :speechtractor)

(defun forums-doc-startp (node path)
  (declare (ignore path))
  (or
    (and (plump:has-attribute node "class")
         (or
           ;; Xenforo
           (cl-ppcre:scan (boundary-regex "message") (plump:attribute node "class"))
           ;; some old ver of phpBB
           (cl-ppcre:scan (boundary-regex "thread_post") (plump:attribute node "class"))
           ;; some Wordpress forums
           (cl-ppcre:scan (boundary-regex "reply") (plump:attribute node "class"))
           ;; these forums have the first post without characteristic DOM
           (cl-ppcre:scan (boundary-regex "topic-author") (plump:attribute node "class"))))))
