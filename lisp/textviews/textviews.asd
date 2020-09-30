(defsystem "textviews"
  :description "A framework for processing of text corpora."
  :version "0.0.0"
  :author "Therminsley"

  :depends-on ("alexandria" "local-time")
  :components ((:module "src"
                :components ((:file "textviews")
                             (:file "source" :depends-on ("textviews"))
                             (:file "processing-layer" :depends-on ("textviews" "source"))
                             (:file "correction" :depends-on ("textviews" "processing-layer"))
                             (:file "text" :depends-on ("source" "processing-layer" "correction"))
                             (:file "division" :depends-on ("text"))
                             (:file "category" :depends-on ("division"))
                             (:file "corpus"
                              :depends-on ("source" "text" "processing-layer" "correction" "division"))
                              (:file "view" :depends-on ("text" "corpus" "division" "category"))
                             ))))
