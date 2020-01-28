(in-package :speechtractor)

(defun general-skip-p (node path)
  (or
    (unless (and (plump:has-attribute node "data-imodoptions");fashionspot wraps posts in such form
                 (equalp "#ModerationSelect option" (plump:attribute node "data-imodoptions")))
      (equalp "form" (plump:tag-name node)))
    (and (plump:has-attribute node "id")
         (or 
           (equalp "copyright" (plump:attribute node "id"))
           (equalp "preFooter" (plump:attribute node "id"))
           (equalp "footer" (plump:attribute node "id"))))   
    (and (plump:has-attribute node "class")
         (or
           (cl-ppcre:scan (boundary-regex "bigFooter")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "button")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "empty-tab")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "modal")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "modal-container")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "fashion-box")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "anon-user-signature")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "sitepoll")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan "(notice|cookie)\\S*(notice|cookie)"
                          (string-downcase (plump:attribute node "class")))
           (cl-ppcre:scan (boundary-regex "login-signup")
                          (plump:attribute node "class"))
           ;; Xenforo's "Click to expand..."
           (cl-ppcre:scan (boundary-regex "bbCodeBlock-expandLink")
                          (plump:attribute node "class"))
           (cl-ppcre:scan (boundary-regex "quoteExpand")
                          (plump:attribute node "class"))))))
