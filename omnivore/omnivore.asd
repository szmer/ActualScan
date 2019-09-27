(defsystem "omnivore"
  :description "A tool for assembling semantic representations."
  :version "0.0.0"
  :author "Therminsley"

  :depends-on ("alexandria" "cl-conllu" "cl-strings" "plump" "trivial-types" "xml-emitter")
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
                              :components ((:file "in-xml") (:file "out-xml"))
                              :depends-on ("logic"))))))
