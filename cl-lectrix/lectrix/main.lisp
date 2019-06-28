(declaim (optimize (debug 3)))
(in-package :cl-lectrix)

;; needed candies:
;; collecting/returning maphash

(defparameter *conll-file* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-lg* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-clear* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/input_utterances.txt.nlp")

(deftype element-origin () '(member :explication :unknown-token :syntax))

(defun weight->purpose (weight)
  (declare (type (weight real)))
  (ecase weight
    (1.0 "explication - definition")
    (0.8 "explication - prototype")
    (0.5 "syntactic connection")))
(defparameter *weight-syntactic-connection* 0.5)

;;
;; We represent a graph as a list of two lists: of nodes and of edges.
;; By convention, the first node in the node list is the root.
;;

(candies:@define-class-with-accessors@ semantic-node ()
    ((label :type symbol) (origin :type element-origin)
      (verb? :type boolean :initform nil)
      ;; important if this is a verb, whether it takes an obj exit:
      (obj-exit? :type boolean :initform nil)))

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
     :verb? (eq universal-pos 'verb)
     :obj-exit? (eq universal-pos 'verb)
     :origin :unknown-token)))

(defun token->semantic-representation (input-token)
  "You get a list of two: a list of nodes and a list of edges."
  (declare (type input-token conllu.rdf::token))
  (let ((explication))
    (or explication
        ;; one node, no edges:
        (list (unknown-token->semantic-node input-token) nil))))

(defun deprel->label (deprel)
  (error "nnt implemented"))

(defun token-tree->semantic-representation (tree)
  (declare (type tree list))
  (let ((token-id->representation (make-hash-table))
        (result-lists (list nil nil))
        ;; We can guarantee that the children will appear later in the tree than their parents.
        (token-order)))
  (do* ((subtrees (rest tree)) ; start with the root's relation
        (current-subtree (pop subtrees) (pop subtrees)))
       ((null subtrees))
    (etypecase subtree
      (string nil)
      (list (setf subtrees (append subtrees subtree)))
      (cl-conllu::token
       (setf (gethash (cl-conllu:token-id subtree) token-id->representation)
                              (token->semantic-representation subtree))
       (push subtree token-order))))
  (dolist (token token-order)
    (when (not (zerop (cl-conllu:token-head token))) ; skip the root obviously
      (push (make-instance 'semantic-edge
                           :from (first (gethash (cl-conllu:token-head token) token-id->representation))
                           :to (first (gethash (cl-conllu:token-id token) token-id->representation))
                           :origin :syntax
                           :label (deprel->label (cl-conllu:token-deprel token))
                           :weight *weight-syntactic-connection*)
            (second result-lists))))
  (maphash (lambda (id representation) ; collect all representations
             (declare (ignore id))
             (setf (first result-lists) ; nodes
                   (cons (first representation) (first result-lists)))
             (setf (second result-lists) ; edges
                   (cons (second representation) (second result-lists))))
           token-id->representation)
  result-lists)
