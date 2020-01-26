(defsystem "speechtractor"
  :description "A tool for extracting semantic document data from HTML pages."
  :version "0.0.1"
  :author "Lookupy"

  :depends-on ("chronicity" "cl-json" "cl-ppcre" "cl-strings" "fiasco" "hunchentoot" "local-time"
               "plump")
  :components ((:file "speechtractor")
               (:file "utils" :depends-on ("speechtractor"))
               (:module "src"
                :components ((:file "paragraph")
                             (:file "classify" :depends-on ("paragraph"))
                             (:file "out" :depends-on ("classify")))
                :depends-on ("utils"))
               (:module "meta-funs"
                :components ((:file "docstart") (:file "author") (:file "date") (:file "permalink")
                             (:file "collections"
                              :depends-on ("docstart" "author" "date" "permalink")))
                :depends-on ("utils"))
               (:module "test"
                :components ((:file "setup")
                             (:file "test-classify" :depends-on ("setup"))
                             (:file "test-docs" :depends-on ("setup"))
                             (:file "test-time" :depends-on ("setup")))
                :depends-on ("src" "meta-funs"))))
