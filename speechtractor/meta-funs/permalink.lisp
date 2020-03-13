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
     (concatenate 'string "#" (plump:attribute node "id")))
    ;; Wallstreetoasis - main question
    ((and (plump:has-attribute node "itemid")
          (plump:has-attribute node "itemtype")
          (equalp "https://schema.org/Question" (plump:attribute node "itemtype")))
     (plump:attribute node "itemid"))
    ;; Wallstreetoasis - comments
    ((and
        (plump:has-attribute node "class")
        (plump:has-attribute node "id")
        (cl-ppcre:scan (boundary-regex "comment") (plump:attribute node "class")))
     ;; technically this uniquely links to the post
     (concatenate 'string "#" (plump:attribute node "id")))))

(defun searchpage-permalink (node path)
  (cond
    ;; Wordpress search
    ((and (equalp "a" (plump:tag-name node))
          (plump:has-attribute node "rel")
          (equalp (plump:attribute node "rel") "bookmark")
          ;; The parent should be a h2.
          (equalp (first (last path 2)) "h2"))
     (plump:attribute node "href"))
    ;; Toscrape test
    ((and (equalp "a" (plump:tag-name node))
          (equalp "(about)" (plump:render-text node)))
     (plump:attribute node "href"))))
