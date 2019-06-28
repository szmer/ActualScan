(defsystem "cl-lectrix"
  :description "A tool for assembling semantic representations."
  :version "0.0.0"
  :author "Therminsley"

  :depends-on ("alexandria" "candies" "cl-conllu")
  :components ((:module "lectrix" :components ((:file "main")
                                               (:file "validation")))
               ))

(defpackage :cl-lectrix
  (:use :cl)
  ;(:export)
  )
