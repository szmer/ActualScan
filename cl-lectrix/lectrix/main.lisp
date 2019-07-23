(declaim (optimize (debug 3)))
(in-package :cl-lectrix)

(defparameter *conll-file* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-lg* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/test_conlls.conll")
(defparameter *conll-file-clear* #p"/home/szymon/lingwy/therminsley/lectrix/spacy_parsing/input_utterances.txt.nlp")
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

(deftype element-origin () '(member
                             :explication-definition
                             :explication-prototype
                             :unknown-token
                             :syntax))
(defparameter *origin->edge-weight*
  (list '(:explication-definition 1.0)
        '(:explication-prototype 0.8)
        '(:unknown-token 0.55)
        '(:syntax 0.5))

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

;; Main semantic "exits" of the structures.

;; NOTE The root from a verbal node defaults to pred.
(setf (symbol-function 'semantic-root) #'first)

(defun designated-subj (graph-lists)
  "Returns a semantic-edge that can be used to plug into the semantic
structure's sub (indicated in the :to property)."
  (declare (type (semantic-edge graph-lists)))
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

(defun unknown-token->semantic-graph (input-token)
  "Get a semantic graph that represents the input-token from a CoNNLU tree,
assuming that we have no definition for that term."
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
                                           :weight (cdr (assoc :syntax *origin->edge-weight*))
                                                          :element-origin :unknown-token)))))
        (propn (let ((nodes (list (make-instance 'semantic-node :label "something"
                                                               :element-origin :unknown-token)
                                  ;; A proper noun doesn't have its label marked as unknown semantic
                                  ;; component, we assume it to be a unique identifier.
                                 (make-instance 'semantic-node :label (cl-conllu:token-lemma input-token)
                                                               :element-origin :unknown-token))))
                (list nodes
                      (list (make-instance 'semantic-edge :from (nth 0 nodes) :to (nth 1 nodes)
                                                          :weight (cdr (assoc :syntax *origin->edge-weight*))
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
                                                         :weight (cdr (assoc :syntax *origin->edge-weight*))
                                                         :element-origin :unknown-token)))))
        (otherwise
         (list (list (make-instance 'semantic-node :label (candies:join-strings
                                                           *prefix-unknown-token*
                                                           (cl-conllu:token-lemma input-token))
                                                   :element-origin :unknown-token))
               (list (make-instance 'semantic-edge :from (nth 0 nodes) :to (nth 1 nodes)
                                                   :weight (cdr (assoc :syntax *origin->edge-weight*))
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

;;
;; Translating from dependency relations to semantic edges.
;;

;; See:
;; https://universaldependencies.org/u/dep/all.html
;; https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md
;;
;; This hashtable needs to show the (semantic) direction of the edge and with which function to
;; create it. If the edge is between two verbal nodes, we need to ensure the proper semantic direction
;; with a proper intermediate `something' node. Otherwise, we ignore the issue ???? (for now).
(defparameter *deprel->edge-spec*
  (alexandria:alist-hash-table
   (list
    '("acl" '(:forward #'designated-obj)) ; clausal modifier tends to be an obj, but we need to consult surroundings in postpro
    '("advcl" '(:backward #'designated-sit)) ; at least judging by UD's examples
    '("advmod" '(:backward #'designated-pred))
    '("agent" '(:forward #'semantic-root)) ; most often "by"?
    '("acomp" '(:forward #'semantic-root))
    '("amod" '(:forward #'semantic-root))
    ;; NOTE probably should be proxied by a they_be_them
    '("appos" '(:forward #'semantic-root))
    '("attr" '(:forward #'semantic-root))
    '("aux" '(:backward #'designated-pred))
    '("auxpass" '(:backward #'designated-pred))
    ;; NOTE coordinating conjunction, should proxy the conj relation in question (postprocessing))
    '("cc" '(:forward #'semantic-root))
    '("conj" '(:forward #'semantic-root))
    '("ccomp" '(:backward #'designated-obj))
    '("compound" '(:forward #'semantic-root))
    '("det" '(:forward #'semantic-root))
    '("dobj" '(:backward #'designated-obj))
    '("mark" '(:forward #'semantic-root)) ; ??
    '("neg" '(:forward #'semantic-root))
    '("npadvmod" '(:forward #'semantic-root)) ; no legitimate case seen
    '("npmod" '(:forward #'semantic-root)) ; no legitimate case seen
    '("nsubj" '(:backward #'designated-subj))
    '("nsubj" '(:backward #'designated-subj)) ; semantically passive, dubious
    '("pcomp" '(:forward #'semantic-root))
    '("pobj" '(:forward #'semantic-root)) ; can also lead to verbals
    '("poss" '(:forward #'semantic-root))
    '("prt" '(:forward #'semantic-root)) ; particle verbs, dubious
    '("prep" '(:forward #'semantic-root))
    '("punct" '(:forward #'semantic-root)) ; dead in practice
    '("relcl" '(:backward #'designated-subj))
    '("subtok" '(:backward #'designated-subj))
    ;; it's possible that we want to plug into sit in verbals? NOTE also through __quote?
    '("xcomp" '(:backward #'designated-obj))
    )
   :test #'equal))

;; See the comment above on *deprel->edge-spec*.
(defun directed-connection (direction edge-fun graph1 graph2)
  "Returns semantic graph lists necessary to connect the two graphs."
  (declare (type keyword direction) (type (function edge-fun)))
  (when (and ([verbal?-of] (semantic-root graph1)) ([verbal?-of] (semantic-root graph2)))
    (let* ((proxy-node (make-instance 'semantic-node :label "something" :origin :syntax))
           (empty-edge (make-instance 'semantic-edge :to proxy-node :label "pred"))
           (main-edge (funcall edge-fun (ccase direction (:forward graph2)
                                               (:backward graph1)))))
      (setf ([from-of] main-edge) proxy-node)
      (setf ([from-of] empty-edge)
            (semantic-root (ccase direction
                             (:forward graph1)
                             (:backward graph2))))
      (list (list proxy-node)
            (list empty-edge main-edge)))
    ;; The simple case, no verbals.
    (list '() (list edge))))

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
    (when (not (zerop (cl-conllu:token-head token))) ; skip the root, obviously
      (let ((connection-graph
              (apply #'directed-connection
                     (append
                      ;; The direction and edge-producing function.
                      (gethash (cl-conllu:token-deprel token) *deprel->edge-spec*)
                      ;; From the head to this token.
                      (list (gethash (cl-conllu:token-head token) token-id->representation)
                            (gethash (cl-conllu:token-id token) token-id->representation)))))))
      (rplacd (graph-nodes result-graph) (cons (graph-nodes connection-graph)
                                               (cdr (graph-nodes result-graph))))
      (setf (graph-edges result-graph) (cons (graph-edges connection-graph)
                                             (graph-edges result-graph)))))
  ;; Collapse all representations.
  (maphash (lambda (id representation)
             (declare (ignore id))
             (setf (graph-nodes result-graph) ; nodes
                   (cons (graph-nodes representation) (graph-nodes result-graph)))
             (setf (graph-edges result-graph) ; edges
                   (cons (graph-edges representation) (graph-edges result-graph))))
           token-id->representation)
  result-graph)
