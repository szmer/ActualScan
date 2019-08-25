(defsystem "omnivore"
  :description "A tool for assembling semantic representations."
  :version "0.0.0"
  :author "Therminsley"

  :depends-on ("alexandria" "cl-conllu" "cl-strings" "trivial-types" "xml-emitter")
  :components ((:module "src"
                :components ((:file "omnivore")
                             (:file "utils" :depends-on ("omnivore"))
                             (:module "logic"
                              :components ((:file "seme")
                                           (:file "graph" :depends-on ("seme"))
                                           (:file "indexing" :depends-on ("graph"))
                                           (:file "representation" :depends-on ("graph"))
                                           (:file "reading" :depends-on ("representation")))
                              :depends-on ("omnivore" "utils"))
                             (:module "output"
                              :components ((:file "xml"))
                              :depends-on ("logic"))))))
