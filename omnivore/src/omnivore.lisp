(defpackage :omnivore (:use :cl :trivial-types :textviews))
(in-package :omnivore)

(defparameter *debug-times* nil)
(defparameter *debug-scoring* t)


(defparameter *filesystem-db-path* #p"~/therminsley/willowseed/db/")
(setf pg-textviews:*db-name* "thermdb")
(setf pg-textviews:*db-username* "therm")
(setf pg-textviews:*db-password* "RHHvVX6Mverv4sMr8Vcz")

(setf drakma:*drakma-default-external-format* :utf-8)

(defparameter *phrase-freq-threshold* 10)
(defparameter *phrase-example-count* 3)

(defparameter *solr-port* 8983)
(defparameter *solr-collection* "lookupy")
(defparameter *solr-analyzed-rows-n* 1000)
(defparameter *solr-snippets-per-doc* 5)

(defparameter *html-path* #p"~/therminsley/lectrix/omnivore/html/")

;; Pg-textviews objects.
;;
;;-(setf *headphones-corp* (pg-textviews:get-corpus "test_headphones_corp"))
;;-(setf *headphones-corp-trees* (pg-textviews:get-corpus "test_headphones_corp_trees"))
;;-(defparameter *comfort-cat*
;;-  (make-category :token :attribute-name "raw_text" :attribute-value "comfort" :criterion :equal))
;;-(defparameter *spec-cat*
;;-  (make-category :sentence :attribute-name "identifier" :attribute-value "s1047/1170313" :criterion :equal))

;;- (defparameter *view* (pg-textviews:get-view *headphones-corp* (list *comfort-cat*) :get-documents t))
;;- (defparameter *view-trees* (pg-textviews:get-view *headphones-corp-trees* (list *comfort-cat*) :get-documents t))

;;-(defparameter *typical*
;;-  (ranked-low (lowest-chunk 0.1
;;-                              (scored-with-average-tfidf (tokens-sents (view-divisions *view*)))
;;-                              (scored-with-length-deviation (tokens-sents (view-divisions *view*))))))
;;-(defparameter *atypical*
;;-  (ranked-high (corrected-with #'/
;;-                             (scored-with-average-tfidf (tokens-sents (view-divisions *view*)))
;;-                             (scored-with-length-deviation (tokens-sents (view-divisions *view*))))))
;;-(defparameter *typical-secs*
;;-  (ranked-low (lowest-chunk 0.1
;;-                              (scored-with-average-tfidf (tokens-section-sents (view-divisions *view*)))
;;-                              (scored-with-length-deviation (tokens-section-sents (view-divisions *view*))))))
;;-(defparameter *atypical-secs*
;;-  (ranked-high (corrected-with #'*
;;-                               (corrected-with #'/
;;-                                               (scored-with-average-tfidf (tokens-section-sents (view-divisions *view*)))
;;-                                               (scored-with-length-deviation (tokens-section-sents (view-divisions *view*))))
;;-                               (scored-with-markers (tokens-section-sents (view-divisions *view*)))) ))


;;;(defparameter *conll-file* #p"~/therminsley/lectrix/spacy_parsing/test_conlls.conll")
;;;(defparameter *conll-file-lg* #p"~/therminsley/lectrix/spacy_parsing/test_conlls.conll")
;;;(defparameter *conll-file-clear* #p"~/therminsley/lectrix/spacy_parsing/input_utterances.txt.nlp")
;;;(defparameter *sents* (cl-conllu:read-conllu *conll-file*))

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


;;;(defparameter *akg-pars* (conll-file-paragraphs-index #p"~/therminsley/lectrix/spacy_parsing/akg_parse.conll"))
