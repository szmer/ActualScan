(in-package :speechtractor)

(defun general-skip-p (node path)
  (and (plump:has-attribute node "class")
       (or 
         (cl-ppcre:scan (boundary-regex "(notice|cookie)\\S*(notice|cookie)")
                        (plump:attribute node "class"))
         ;; Xenforo's "Click to expand..."
         (cl-ppcre:scan (boundary-regex "bbCodeBlock-expandLink")
                        (plump:attribute node "class")))))
