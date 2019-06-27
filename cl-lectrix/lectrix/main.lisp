(declaim (optimize (debug 3)))
(in-package :cl-lectrix)

(defparameter *conll-file* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-lg* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-clear* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/input_utterances.txt.nlp")

(deftype element-origin () '(member :explication :unknown-token))

(defun weight->purpose (weight)
  (declare (type (weight real)))
  (ecase weight
    (1.0 "explication - definition")
    (0.8 "explication - prototype")
    (0.5 "syntactic connection")))

(candies:@define-class-with-accessors@ semantic-node ()
    ((label :type symbol) (origin :type element-origin)))

(candies:@define-class-with-accessors@ semantic-edge ()
    ((label :type symbol) (origin :type element-origin)
      (weight :type real)
      (from :type semantic-node) (to :type semantic-node)))

;(defparameter *sents* (cl-conllu:read-conllu *conll-file*))

;;;(cl-conllu:sentence-tokens (fifth *sents*))
;;;(cl-conllu:sentence-binary-tree (fifth *sents*))
;;;(conllu.draw:tree-sentence (fourth *sents*))

(defparameter *prefix-unknown-token* "??;")
(defparameter *prefix-proper-name-token* "==;")

(defun unknown-token->semantic-node (input-token)
  (declare (type input-token conllu.rdf::token))
  (let* ((universal-pos (candies:string->symbol (cl-conllu:token-upostag input-token)))))
  (unless (find proper-label (list 'x 'sym 'punct 'intj))
    (make-instance
     'semantic-node
     :label (case universal-pos
              (verb
               (candies:join-strings "_they_" *prefix-unknown-token*
                                     (cl-conllu:token-lemma input-token)
                                     "_them"))
              (propn ; "proper noun"
               (candies:join-strings *prefix-proper-name-token*
                                     (cl-conllu:token-lemma input-token)))
              (otherwise
               (candies:join-strings *prefix-unknown-token*
                                     (cl-conllu:token-lemma input-token))))
     :origin :unknown-token)))

(defun token->semantic-node (input-token)
  (declare (type input-token conllu.rdf::token))
  (let ((explication))
    (or explication (unknown-token->semantic-node input-token))))
