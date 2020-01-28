(in-package :speechtractor)

(defun forums-permalink (node path)
  (cond
    ;; Xenforo
    ((and (equalp "a" (plump:tag-name node))
          (plump:has-attribute node "title")
          (cl-ppcre:scan "[pP]erma-?link" (plump:attribute node "title"))
          (plump:has-attribute node "href"))
     (plump:attribute node "href"))
    ;; some Xenforo (styleforum)
    ((and (equalp "a" (plump:tag-name node))
          (plump:has-attribute node "data-xf-init")
          (equalp "share-tooltip" (plump:attribute node "data-xf-init"))
          (plump:has-attribute node "href"))
     (plump:attribute node "href"))
    ;; some old ver of phpBB
    ((and (equalp "a" (plump:tag-name node))
          (plump:has-attribute node "class")
          (plump:has-attribute node "href")
          (cl-ppcre:scan "[pP]erma-?link" (plump:attribute node "class")))
     (plump:attribute node "href"))
    ;; some Wordpress forums
    ((and (equalp "a" (plump:tag-name node))
          (typep node 'plump:child-node)
          (plump:has-attribute (plump:parent node) "class")
          (cl-ppcre:scan (boundary-regex "post-time")
                         (plump:attribute (plump:parent node) "class"))
          (plump:has-attribute node "href"))
     (plump:attribute node "href"))
    ;; thestudentroom
    ((and 
        (plump:has-attribute node "class")
        (plump:has-attribute node "id")
        ;; thestudentroom
        (cl-ppcre:scan (boundary-regex "post") (plump:attribute node "class"))
        (typep node 'plump:child-node)
        (plump:has-attribute (plump:parent node) "id")
        (equalp "postsContainer" (plump:attribute (plump:parent node) "id")))
     ;; technically this uniquely links to the post
     (concatenate 'string "#" (plump:attribute node "id")))))
