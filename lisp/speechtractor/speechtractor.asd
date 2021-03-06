(defsystem "speechtractor"
  :description "A tool for extracting semantic document data from HTML pages."
  :version "0.0.1"
  :author "Lookupy"

  :depends-on ("chronicity" "cl-json" "cl-ppcre" "cl-strings" "fiasco" "hunchentoot" "local-time"
               "plump" "log4cl" "purl"
               ;; For testing http.
               "drakma")
  :components ((:file "speechtractor")
               (:file "utils" :depends-on ("speechtractor"))
               (:module "meta-funs"
                :components ((:file "docstart") (:file "author") (:file "date") (:file "permalink")
                             (:file "skip") (:file "search") (:file "meta-burner" :depends-on ("date"))
                             (:file "collections"
                              :depends-on ("docstart" "author" "date" "permalink" "skip" "search" "meta-burner")))
                :depends-on ("utils"))
               (:module "src"
                :components ((:file "paragraph")
                             (:file "classify" :depends-on ("paragraph"))
                             (:file "out" :depends-on ("classify"))
                             ;; The http server makes use of functions defined in meta-funs.
                             (:file "http" :depends-on ("out")))
                :depends-on ("utils" "meta-funs"))
               (:module "test"
                :components ((:file "setup")
                             (:file "test-classify" :depends-on ("setup"))
                             (:file "test-docs" :depends-on ("setup"))
                             (:file "test-time" :depends-on ("setup"))
                             (:file "test-http" :depends-on ("setup"))
                             (:file "test-realworld" :depends-on ("setup")))
                :depends-on ("src" "meta-funs"))))
