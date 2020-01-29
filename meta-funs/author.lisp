(in-package :speechtractor)

(defun forums-author (node path)
  (cond
    ;; Xenforo
    ((and
       (plump:has-attribute node "class")
       (cl-ppcre:scan (boundary-regex "message") (plump:attribute node "class"))
       (plump:has-attribute node "data-author"))
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

(defun media-author (node path)
  (cond
    ((and (plump:has-attribute node "class")
          (cl-ppcre:scan (boundary-regex "contributor-name") (plump:attribute node "class")))
     (plump:render-text node))))

(defun blog-author (node path)
  (cond
    ((and (plump:has-attribute node "class")
          ;; Wordpress
          (cl-ppcre:scan (boundary-regex "entry-author-name") (plump:attribute node "class")))
     (server-debug-print (plump:render-text node))
     (plump:render-text node))))
