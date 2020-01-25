(defsystem "speechtractor"
  :description "A tool for extracting semantic document data from HTML pages."
  :version "0.0.1"
  :author "Lookupy"

  :depends-on ("cl-ppcre" "cl-strings" "fiasco" "hunchentoot" "plump" "purl")
  :components ((:module "src"
                :components ((:file "speechtractor")
                             (:file "utils" :depends-on ("speechtractor"))
                             (:file "paragraph" :depends-on ("utils"))
                             (:file "classify" :depends-on ("paragraph"))
                             ))
               (:module "test"
               :components ((:file "setup")
                            (:file "test-classify" :depends-on ("setup")))
               :depends-on ("src"))))
