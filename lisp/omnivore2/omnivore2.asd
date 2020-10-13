(defsystem "omnivore2"
  :description "Computing features for text, coupled with Solr."
  :version "0.0.1"
  :author "ActualScan"

  :depends-on ("alexandria" "cl-json" "cl-ppcre" "cl-strings" "fiasco" "hunchentoot" "local-time" "log4cl"
                            "py4cl" "drakma" "babel")
  :components ((:file "omnivore2")
               (:file "utils" :depends-on ("omnivore2"))
               (:module "src"
                :components ((:file "python")
                             (:file "period" :depends-on ("python"))
                             (:file "stationary-analysis" :depends-on ("python" "period"))
                             (:file "contextual-analysis" :depends-on ("python" "period"))
                             (:file "construct-solr-query" :depends-on ("stationary-analysis"))
                             (:file "retrieve-context" :depends-on ("construct-solr-query"))
                             (:file "solr-submit")
                             (:file "http" :depends-on ("stationary-analysis"
                                                        "retrieve-context")))
                :depends-on ("utils"))
               (:module "test"
                :components ((:file "setup")
                             (:file "period" :depends-on ("setup"))
                             (:file "stationary-analysis" :depends-on ("setup"))
                             (:file "contextual-analysis" :depends-on ("setup")))
                :depends-on ("src"))
               ))
