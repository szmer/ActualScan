(in-package :speechtractor)

(defun general-skip-p (node path)
  (or
    (and (plump:has-attribute node "id")
         (or 
           (equalp "copyright" (plump:attribute node "id"))
           (equalp "preFooter" (plump:attribute node "id"))
           (equalp "footer" (plump:attribute node "id"))))   
    (and (plump:has-attribute node "class")
         (or
           (cl-ppcre:scan (boundary-regex "button")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "sitepoll")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "empty-tab")
                          (plump:attribute node "class")) 
           (cl-ppcre:scan (boundary-regex "(notice|cookie)\\S*(notice|cookie)")
                          (plump:attribute node "class"))
           (cl-ppcre:scan (boundary-regex "login-signup")
                          (plump:attribute node "class"))
           ;; Xenforo's "Click to expand..."
           (cl-ppcre:scan (boundary-regex "bbCodeBlock-expandLink")
                          (plump:attribute node "class"))))))
