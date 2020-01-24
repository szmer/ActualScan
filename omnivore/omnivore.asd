(defsystem "omnivore"
  :description "A tool for assembling semantic representations."
  :version "0.0.0"
  :author "Therminsley"

  :depends-on ("alexandria" "babel" "cl-conllu" "cl-json" "cl-stopwatch" "cl-strings"
               "drakma" "html-template" "hunchentoot" "plump" "purl" "trivial-types"
               "xml-emitter" "yason" "textviews" "pg-textviews")
  :components ((:module "src"
                :components ((:file "omnivore")
                             (:file "utils" :depends-on ("omnivore"))
                             (:module "logic"
                              :components ((:file "seme")
                                           (:file "graph" :depends-on ("seme"))
                                           (:file "representation" :depends-on ("graph"))
                                           (:file "indexing" :depends-on ("representation"))
                                           (:file "reading" :depends-on ("representation")))
                              :depends-on ("omnivore" "utils"))
                             (:module "io"
                              :components ((:file "in-xml") (:file "out-xml")
                                           (:file "quickdb" :depends-on ("in-xml"))
                                           (:file "in-textviews")
                                           (:file "in-solr"))
                              :depends-on ("omnivore" "logic"))
                             (:module "raw"
                              :components ((:file "stopwords")
                                           (:file "scoring")
                                           (:file "collocations" :depends-on ("stopwords")))
                              :depends-on ("omnivore" "utils" "logic" "io"))
                             ))
                (:module "biz"
                 :components ((:file "basic")
                              (:file "main" :depends-on ("basic"))
                              (:file "server" :depends-on ("main")))
                 :depends-on ("src"))))
