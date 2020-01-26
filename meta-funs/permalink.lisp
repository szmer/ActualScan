(in-package :speechtractor)

(defun forums-permalink (node path)
  (cond
    ;; Xenforo
    ((and (plump:has-attribute node "title")
          (plump:has-attribute node "href")
          (cl-ppcre:scan "[pP]erma-?link" (plump:attribute node "title")))
     (plump:attribute node "href"))
    ;; some old ver of phpBB
    ((and (plump:has-attribute node "class")
          (plump:has-attribute node "href")
          (cl-ppcre:scan "[pP]erma-?link" (plump:attribute node "class")))
     (plump:attribute node "href"))))
