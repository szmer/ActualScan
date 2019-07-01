(declaim (optimize (debug 3)))
(in-package :cl-lectrix)

;; needed candies:
;; collecting/returning maphash

(defparameter *conll-file* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-lg* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-clear* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/input_utterances.txt.nlp")
;;;(defparameter *sents* (cl-conllu:read-conllu *conll-file*))
;;;(cl-conllu:sentence-tokens (fifth *sents*))
;;;(cl-conllu:sentence-binary-tree (fifth *sents*))
;;;(conllu.draw:tree-sentence (fourth *sents*))

(deftype element-origin () '(member :explication :unknown-token :syntax))

(defun weight->purpose (weight)
  (declare (type (weight real)))
  (ecase weight
    (1.0 "explication - definition")
    (0.8 "explication - prototype")
    (0.5 "syntactic connection")))
(defparameter *weight-syntactic-connection* 0.5)

(candies:@define-class-with-accessors@ semantic-node ()
    ((label :type symbol) (origin :type element-origin)
      (verbal? :type boolean :initform nil)
      ;; important if this is a verbal, whether it takes an obj exit:
      (obj-exit? :type boolean :initform nil)))

(candies:@define-class-with-accessors@ semantic-edge ()
    ((label :type string :initform "") (origin :type element-origin)
      (weight :type real)
      (from :type semantic-node) (to :type semantic-node)))

;;
;; We represent a graph as a list of two lists: of nodes and of edges.
;; By convention, the first node in the node list is the root.
;;

(defparameter *prefix-unknown-token* "??;")
(defparameter *prefix-proper-name-token* "==;")

(setf (symbol-function 'graph-nodes) #'first) ; wrap functions semantically
(setf (symbol-function 'graph-edges) #'second)
;; TODO TODO probably the root for verbs is their pred??
(setf (symbol-function 'semantic-root) #'first)

(defun designated-subj (graph-lists)
  nil)

(defun designated-obj (graph-lists)
  nil)

(defun designated-pred (graph-lists)
  nil)

(defun designated-sit (graph-lists)
  nil)

;;
;; Translation from CoNLL.
;;

;; See:
;; https://universaldependencies.org/u/dep/all.html
;; https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md
(defparameter *deprel->graph-places*
  (alexandria:alist-hash-table
   (list
    "advmod" '(#'designated-pred #'semantic-root)
    "amod" '(#'semantic-root #'semantic-root)
    "det" '(#'semantic-root #'semantic-root)
    "dobj" '(#'designated-obj #'semantic-root)
    "mark" '(#'semantic-root #'semantic-root) ; ??
    "neg" '(#'semantic-root #'semantic-root)
    "nsubj" '(#'designated-subj #'semantic-root)
    "pcomp" '(#'semantic-root #'semantic-root)
    "pobj" '(#'semantic-root #'semantic-root) ; can also lead to verbals
    "prep" '(#'semantic-root #'semantic-root)
    "relcl" '(#'designated-subj #'semantic-root)
    ))
   :test #'equal))

(defun unknown-token->semantic-graph (input-token)
  (declare (type input-token conllu.rdf::token))
  (let* ((universal-pos (candies:string->symbol (cl-conllu:token-upostag input-token)))))
  (if (find universal-pos '(x sym punct intj aux part)) ; part is controversial, gotta extract the possessive somehow
      (case universal-pos
        (verb (list
               (list (make-instance
                      'semantic-node
                      :label (candies:join-strings "_they_" *prefix-unknown-token*
                                                    (cl-conllu:token-lemma input-token)
                                                    "_them")
                      :verbal? t :obj-exit? t :element-origin :unknown-token))
               ()))
        (noun (let ((nodes (list (make-instance 'semantic-node :label "something"
                                                               :element-origin :unknown-token)
                                 (make-instance 'semantic-node :label (candies:join-strings
                                                                       *prefix-unknown-token*
                                                                       (cl-conllu:token-lemma input-token))
                                                               :element-origin :unknown-token))))
                (list nodes
                      (list (make-instance 'semantic-edge :from (nth 0 nodes) :to (nth 1 nodes)
                                           :weight *weight-syntactic-connection*
                                                          :element-origin :unknown-token)))))
        (propn (let ((nodes (list (make-instance 'semantic-node :label "something"
                                                               :element-origin :unknown-token)
                                  ;; A proper noun doesn't have its label marked as unknown semantic
                                  ;; component, we assume it to be a unique identifier.
                                 (make-instance 'semantic-node :label (cl-conllu:token-lemma input-token)
                                                               :element-origin :unknown-token))))
                (list nodes
                      (list (make-instance 'semantic-edge :from (nth 0 nodes) :to (nth 1 nodes)
                                                          :weight *weight-syntactic-connection*
                                                          :element-origin :unknown-token)))))
        (adj (let ((nodes (list (make-instance 'semantic-node :label "__they_be_"
                                                              :verbal? t
                                                              :element-origin :unknown-token)
                                (make-instance 'semantic-node :label (candies:join-strings
                                                                      *prefix-unknown-token*
                                                                      (cl-conllu:token-lemma input-token))
                                                              :element-origin :unknown-token))))
               (list nodes
                     (list (make-instance 'semantic-edge :from (nth 0 nodes) :to (nth 1 nodes)
                                                         :weight *weight-syntactic-connection*
                                                         :element-origin :unknown-token)))))
        (otherwise
         (list (list (make-instance 'semantic-node :label (candies:join-strings
                                                           *prefix-unknown-token*
                                                           (cl-conllu:token-lemma input-token))
                                                   :element-origin :unknown-token))
               (list (make-instance 'semantic-edge :from (nth 0 nodes) :to (nth 1 nodes)
                                                   :weight *weight-syntactic-connection*
                                                   :element-origin :unknown-token)))))
    ;; for a null representation:
    (list nil nil)))

(defun token->semantic-representation (input-token)
  "You get a list of two: a list of nodes and a list of edges."
  (declare (type input-token conllu.rdf::token))
  (let ((explication))
    (or explication
        ;; one node, no edges:
         (unknown-token->semantic-graph input-token))))

(defun deprel->label (deprel)
  (error "not implemented"))

(defun token-tree->semantic-representation (tree)
  (declare (type tree list))
  (let ((token-id->representation (make-hash-table))
        (result-graph (list nil nil))
        ;; We can guarantee that the children will appear later in the tree than their parents.
        (token-order)))
  ;; Collecting unit representations.
  (do* ((subtrees (rest tree)) ; start with the root's relation (we don't use string deprels)
        (current-subtree (pop subtrees) (pop subtrees)))
       ((null subtree))
    (etypecase subtree
      (string nil)
      (list (setf subtrees (append subtrees subtree)))
      (cl-conllu::token
       (setf (gethash (cl-conllu:token-id subtree) token-id->representation)
                              (token->semantic-representation subtree))
       (push subtree token-order))))
  ;; Edges between units.
  (dolist (token token-order)
    (when (not (zerop (cl-conllu:token-head token))) ; skip the root obviously
      (push (make-instance 'semantic-edge
                           :from (semantic-root (gethash (cl-conllu:token-head token) token-id->representation))
                           :to (semantic-root (gethash (cl-conllu:token-id token) token-id->representation))
                           :element-origin :syntax
                           :label (deprel->label (cl-conllu:token-deprel token))
                           :weight *weight-syntactic-connection*)
            (graph-edges result-graph))))
  ;; Collapse all representations.
  (maphash (lambda (id representation)
             (declare (ignore id))
             (setf (graph-nodes result-graph) ; nodes
                   (cons (graph-nodes representation) (graph-nodes result-graph)))
             (setf (graph-edges result-graph) ; edges
                   (cons (graph-edges representation) (graph-edges result-graph))))
           token-id->representation)
  result-graph)
