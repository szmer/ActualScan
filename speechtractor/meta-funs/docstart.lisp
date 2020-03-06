(in-package :speechtractor)

(defun full-html-doc-startp (node path)
  (equalp "html" (plump:tag-name node)))

(defun forums-doc-startp (node path)
  (declare (ignore path))
  (or
    (and (plump:has-attribute node "class")
         (or
           ;; Xenforo
           (cl-ppcre:scan (boundary-regex "message") (plump:attribute node "class"))
           ;; some old ver of phpBB
           (cl-ppcre:scan (boundary-regex "thread_post") (plump:attribute node "class"))
           ;; Wallstreetoasis
           (cl-ppcre:scan (boundary-regex "comment") (plump:attribute node "class"))
           ))
    (and (plump:has-attribute node "typeof")
         ;; Wallstreetoasis first post
         (cl-ppcre:scan (boundary-regex "sioct:BoardPost") (plump:attribute node "typeof")))
    (and (equalp "div" (plump:tag-name node))
         (plump:has-attribute node "class")
         (or
           ;; some Wordpress forums
           (cl-ppcre:scan (boundary-regex "reply") (plump:attribute node "class"))
           ;; (these forums have the first post without characteristic DOM)
           (cl-ppcre:scan (boundary-regex "topic-author") (plump:attribute node "class"))))
    (and
      (plump:has-attribute node "class")
      ;; thestudentroom
      (cl-ppcre:scan (boundary-regex "post") (plump:attribute node "class"))
      (typep node 'plump:child-node)
      (plump:has-attribute (plump:parent node) "id")
      (equalp "postsContainer" (plump:attribute (plump:parent node) "id")))))
