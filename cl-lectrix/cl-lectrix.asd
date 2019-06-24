(defsystem "cl-lectrix"
  :description "A tool for assembling semantic representations."
  :version "0.0.0"
  :author "Therminsley"

  :depends-on ("cl-conllu")
  :components ((:module "lectrix" :components ((:file "main")))
               ))

(defpackage :cl-lectrix
  (:use :cl)
  ;(:export)
  )
