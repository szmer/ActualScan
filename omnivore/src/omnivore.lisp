(defpackage :omnivore (:use :cl :trivial-types))
(in-package :omnivore)

(defparameter *conll-file* #p"~/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-lg* #p"~/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-clear* #p"~/therminsley/lectrix/spacy_parsing/input_utterances.txt.nlp")
(defparameter *sents* (cl-conllu:read-conllu *conll-file*))
;;;(cl-conllu:sentence-tokens (fifth *sents*))
;;;(cl-conllu:sentence-binary-tree (fifth *sents*))
;;;(conllu.draw:tree-sentence (fourth *sents*))
;;;
;;;(remove-duplicates (reduce #'append (mapcar (lambda (s) (mapcar (lambda (tk) (cl-conllu:token-deprel tk)) (cl-conllu:sentence-tokens s))) *sents*)) :test #'equalp)
;;; -> ("prt" "acl" "poss" "attr" "relcl" "xcomp" "ccomp" "nsubjpass" "auxpass"
;;; "pcomp" "agent" "subtok" "appos" "aux" "mark" "advcl" "acomp" "dobj" "nsubj"
;;; "advmod" "amod" "compound" "npadvmod" "punct" "neg" "ROOT" "cc" "conj" "prep"
;;; "det" "pobj")

;;;(defparameter *a* (create-graph :syntax (list "something" "nothing") (list '(1 0))))
;;;(defparameter *b* (sentence->representation (fifth *sents*)))

;;;(defparameter *test-sents* (cl-conllu:read-conllu #p"~/therminsley/lectrix/spacy_parsing/bigparse.conll"))
;;;(defparameter *test-query* (conll-sentences-query-index *test-sents* "compact"))
;;; -> (#<HASH-TABLE :TEST EQUALP :COUNT 125 {104B9BF793}> #<HASH-TABLE :TEST EQUALP :COUNT 24 {104B9BEE33}>)
;;;(defparameter *test-query* (conll-sentences-query-index *test-sents* "treble"))
;;; -> (#<HASH-TABLE :TEST EQUALP :COUNT 1202 {10029EA3B3}> #<HASH-TABLE :TEST EQUALP :COUNT 257 {10029DFCB3}>)

;;;OMNIVORE> (defparameter *gen-pars* (conll-file-paragraphs-index #p"~/lingwy/therminsley/lectrix/spacy_parsing/general_parse.conll"))
;;;*GEN-PARS*
;;;OMNIVORE> (defparameter *gen-query* (paragraph-index-query *gen-pars* "Sennheiser"))
;;;*GEN-QUERY*
;;;OMNIVORE> (defparameter *gen-query-cliques* (index-strong-cliques *gen-query*))
;;;*GEN-QUERY-CLIQUES*
;;;OMNIVORE> (index-pprint-cliques *gen-query-cliques* :minimum-size 5)
