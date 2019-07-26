(defsystem "cl-lectrix"
  :description "A tool for assembling semantic representations."
  :version "0.0.0"
  :author "Therminsley"

  :depends-on ("alexandria" "candies" "cl-conllu" "cl-strings" "trivial-types")
  :components ((:module "lectrix" :components ((:file "main")))
               ))
