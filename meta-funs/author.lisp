(in-package :speechtractor)

(defun forums-author (node path)
  (cond
    ;; Xenforo
    ((plump:has-attribute node "data-author")
     (plump:attribute node "data-author")) 
    ;; some old ver of phpBB
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "postauthor") (plump:attribute node "class")))
     (plump:render-text node))
    ;; some Wordpress forums
    ((and (equalp "a" (plump:tag-name node))
          (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "user-name") (plump:attribute node "class")))
     (plump:render-text node))
    ;; Wallstreetoasis
    ((and (plump:has-attribute node "itemprop")
         ;; Wallstreetoasis first post
         (equalp "author" (plump:attribute node "itemprop")))
     (plump:render-text node))
    ;; thestudentroom
    ((and (equalp "a" (plump:tag-name node))
          (plump:has-attribute node "data-userid")
          (typep node 'plump:child-node)
          (typep (plump:parent node) 'plump:child-node)
          (plump:has-attribute (plump:parent (plump:parent node)) "class")
          (cl-ppcre:scan (boundary-regex "post-user")
                         (plump:attribute (plump:parent (plump:parent node)) "class")))
     (plump:render-text node))))
