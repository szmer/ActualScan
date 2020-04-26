;;;; The boolean functions for determining if a link on a search page leads to another search pages.

(in-package :speechtractor)

(defun searchpage-searchp (node path)
  ;; We need to translate it to explicit boolean for Python.
  (not
   (null
    (and (plump:has-attribute node "class")
         (or
          ;; Test quotes.toscrape.com
          (cl-ppcre:scan (boundary-regex "next") (plump:attribute node "class")))))))
