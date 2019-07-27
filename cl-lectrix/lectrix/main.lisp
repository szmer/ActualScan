(declaim (optimize (debug 3)))

(defpackage :cl-lectrix (:use :cl :trivial-types))
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

;;;
;;; Auxillary types.
;;;
(deftype element-creator () '(member
                             :explication-definition
                             :explication-prototype
                             :unknown-token
                             :syntax))
(defparameter *creator->stalk-weight*
  (alexandria:alist-hash-table
   (list '(:explication-definition 1.0)
         '(:explication-prototype 0.8)
         '(:unknown-token 0.55)
         '(:syntax 0.5))))

(deftype direction () '(member :from :to))
(defun opposite (direction)
  (declare (type direction direction))
  (ecase direction (:from :to) (:to :from)))

;;;
;;; Graph components.
;;;

(defclass seme ()
  ((label :reader seme-label :initarg :label :type symbol)
   (creator :reader seme-creator :initarg :creator :type element-creator)))

(defclass berry (seme)
  ((verbalp :reader berry-verbalp :initarg verbalp :initform nil :type boolean)
   ;; important if this is a verbal, whether it takes an obj exit:
   (obj-exit-p :reader berry-obj-exit-p :initarg obj-exit-p :initform nil :type boolean)))

(defclass stalk (seme)
    ((weight :reader stalk-weight :initarg :weight :type real)
     (from :accessor stalk-from :initarg :from :type berry)
     (to :accessor stalk-to :initarg :to :type berry)))

(defun stalk-connect (direction stalk target-berry)
  (case direction
    (:from (setf (stalk-from stalk) target-berry) stalk)
    (:to (setf (stalk-to stalk) target-berry) stalk)))

;;
;; We represent a graph as a list of two lists: of berries and of stalks.
;; By convention, the first berry in the berry list is the root.
;;

(defparameter *prefix-unknown-token* "??;")
(defparameter *prefix-proper-name-token* "==;")

(defclass graph ()
  ((berries :accessor graph-berries :initarg :berries :initform nil
            :type (proper-list berry))
   (stalks :accessor graph-stalks :initarg :stalks :initform nil
           :type (proper-list stalks))))

(defun create-graph (creator berry-specs stalk-specs)
  "Intended for new graphs. The creator applies for all created semes, and also
for determining edge weights. Berries are defined by labels (optionally in a
list with verbalp and obj-exit-p) and stalks by berry indices."
  (let ((berries (mapcar
                  (lambda (berry-spec)
                    (let ((berry-spec (if (listp berry-spec) berry-spec
                                          (list berry-spec))))
                      (make-instance 'berry
                                     :label (first berry-spec)
                                     :creator creator
                                     :verbalp (second berry-spec)
                                     :obj-exit-p (third berry-spec))))
                  berry-specs)))
    (make-instance 'graph
                   :berries berries
                   :stalks (mapcar
                            (lambda (stalk-spec)
                              (make-instance 'stalk
                                             :from (elt berries (first stalk-spec))
                                             :to (elt berries (second stalk-spec))
                                             :creator creator
                                             :weight (gethash creator *creator->stalk-weight*)))
                            stalk-specs))))

(defun join-graphs (&rest graphs)
  (make-instance 'graph
                 :berries (reduce #'append (mapcar #'graph-berries graphs))
                 :stalks (reduce #'append (mapcar #'graph-stalks graphs))))

(defun graph-root (graph) (first (graph-berries graph)))

;;
;; Main semantic "exits" of the structures.
;;
;;;===
;;;===;; NOTE The root from a verbal berry defaults to pred.
;;;===(defun graph-root-stalk (creator direction graph)
;;;===  )
;;;===
;;;===(defun graph-subj-stalk (creator direction graph)
;;;===  "Returns a stalk that can be used to plug into the semantic structure's sub
;;;===(indicated in the :to property)."
;;;===  (declare (type (creator direction graph creator direction graph)))
;;;===  nil)
;;;===
;;;===(defun graph-obj-stalk (creator direction graph)
;;;===  nil)
;;;===
;;;===(defun graph-pred-stalk (creator direction graph)
;;;===  nil)
;;;===
;;;===(defun graph-sit-stalk (creator direction graph)
;;;===  nil)

;;
;; Translation from CoNLL.
;;

(defun unknown-token->graph (input-token)
  "Get a semantic graph that represents the input-token from a CoNNLU tree,
assuming that we have no definition for that term."
  (declare (type conllu.rdf::token input-token))
  (let ((universal-pos (cl-conllu:token-upostag input-token)))
    (apply
     (alexandria:curry #'create-graph :unknown-token)
     ;; part is controversial here, gotta extract the possessive somehow
     (if (find universal-pos '("x" "sym" "punct" "intj" "aux" "part") :test #'equalp)
         ;; for a null representation:
         '(() ())
         ;; otherwise build a proper graph:
         (case universal-pos
           (verb (list
                  (list (cl-strings:join
                         (list "_they_" *prefix-unknown-token*
                               (cl-conllu:token-lemma input-token)
                               "_them"))
                        :verbalp :obj-exit-p)
                  ()))
           (noun (list
                  (list "something"
                        (cl-strings:join (list
                                          *prefix-unknown-token*
                                          (cl-conllu:token-lemma input-token))))
                  '((0 1))))
           (propn (list
                   (list "something"
                         ;; A proper noun doesn't have its label marked as unknown semantic
                         ;; component, we assume it to be a unique identifier.
                         (cl-conllu:token-lemma input-token))
                   '((0 1))))
           (adj (list
                 (list "__they_be_"
                       (cl-strings:join (list *prefix-unknown-token*
                                              (cl-conllu:token-lemma input-token))))
                 '((0 1))))
           (otherwise (list
                       (list (cl-strings:join
                              (list *prefix-unknown-token*
                                    (cl-conllu:token-lemma input-token))))
                       '((0 1)))))))))

(defun token->representation (input-token)
  "You get a list of two: a list of berries and a list of stalks."
  (declare (type conllu.rdf::token input-token))
  (let ((official-form))
    (or official-form ; don't expand to the explanation on default
        (unknown-token->graph input-token))))

;;
;; Translating from dependency relations to semantic stalks.
;;

;; See:
;; https://universaldependencies.org/u/dep/all.html
;; https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md
;;
;; This hashtable needs to show the (semantic) direction of the stalk and with which function to
;; create it.
(defparameter *deprel->stalk-spec*
  (alexandria:alist-hash-table
   (list
    '("acl" '(:forward #'designated-obj)) ; clausal modifier tends to be an obj, but we need to consult surroundings in postpro
    '("advcl" '(:backward #'designated-sit)) ; at least judging by UD's examples
    '("advmod" '(:backward #'designated-pred))
    '("agent" '(:forward #'graph-root)) ; most often "by"?
    '("acomp" '(:forward #'graph-root))
    '("amod" '(:forward #'graph-root))
    ;; NOTE probably should be proxied by a they_be_them
    '("appos" '(:forward #'graph-root))
    '("attr" '(:forward #'graph-root))
    '("aux" '(:backward #'designated-pred))
    '("auxpass" '(:backward #'designated-pred))
    ;; NOTE coordinating conjunction, should proxy the conj relation in question (postprocessing))
    '("cc" '(:forward #'graph-root))
    '("conj" '(:forward #'graph-root))
    '("ccomp" '(:backward #'designated-obj))
    '("compound" '(:forward #'graph-root))
    '("det" '(:forward #'graph-root))
    '("dobj" '(:backward #'designated-obj))
    '("mark" '(:forward #'graph-root)) ; ??
    '("neg" '(:forward #'graph-root))
    '("npadvmod" '(:forward #'graph-root)) ; no legitimate case seen
    '("npmod" '(:forward #'graph-root)) ; no legitimate case seen
    '("nsubj" '(:backward #'designated-subj))
    '("nsubj" '(:backward #'designated-subj)) ; semantically passive, dubious
    '("pcomp" '(:forward #'graph-root))
    '("pobj" '(:forward #'graph-root)) ; can also lead to verbals
    '("poss" '(:forward #'graph-root))
    '("prt" '(:forward #'graph-root)) ; particle verbs, dubious
    '("prep" '(:forward #'graph-root))
    '("punct" '(:forward #'graph-root)) ; dead in practice
    '("relcl" '(:backward #'designated-subj))
    '("subtok" '(:backward #'designated-subj))
    ;; it's possible that we want to plug into sit in verbals? NOTE also through __quote?
    '("xcomp" '(:backward #'designated-obj))
    )
   :test #'equal))

;; If the stalk is between two verbal berries, we need to ensure the proper semantic direction
;; with a proper intermediate `something` berry. Otherwise, we ignore the issue ???? (for now).
(defun connection-graph (stalk-fun direction from-graph to-graph)
  "Returns a graph necessary to connect the two graphs."
  (declare (type keyword direction) (type function stalk-fun)
           (type graph from-graph to-graph))
  (let ((main-stalk (funcall stalk-fun :syntax direction to-graph)))
    (if (and (berry-verbalp (graph-root from-graph))
             (berry-verbalp (graph-root to-graph)))
        (let ((proxy-berry (make-instance 'berry :label "something" :creator :syntax))
              (proxy-stalk (graph-pred-stalk :syntax (opposite direction) from-graph)))
          (make-instance 'graph
                         :berries (list proxy-berry)
                         ;; Connect the proxy berry to the main graphs.
                         :stalks (list (stalk-connect direction proxy-stalk proxy-berry)
                                       (stalk-connect (opposite direction) main-stalk
                                                      proxy-berry))))
        ;; The simple case, no verbals.
        (make-instance 'graph
                       :berries ()
                       :stalks (list
                                (stalk-connect
                                 (opposite direction)
                                 main-stalk
                                 (graph-root from-graph)))))))

(defun token-tree->representation (tree)
  "The function expects the output from cl-conllu:sentence-binary-tree."
  (declare (type list tree))
  (let ((token-id->representation (make-hash-table))
        ;; The properly ordered list of token objects (guarantee that the children will appear
        ;; later in the tree than their parents). The first item will be the root.
        (token-order))
    ;; Collect unit representations, descending down the tree in depth-first fashion.
    (do* ((subtrees (rest tree)) ; start with the root's relation (we don't use string deprels)
          (current-subtree (pop subtrees) (pop subtrees)))
         ((null current-subtree))
      (etypecase current-subtree
        (string nil) ; the in-tree deprel annotations, ignore them
        (list (setf subtrees (append subtrees current-subtree))) ; intermediate nodes
        (cl-conllu::token ; leaves - retrieve the representation, place in the node order
         (setf (gethash (cl-conllu:token-id current-subtree) token-id->representation)
               (token->representation current-subtree))
         (push current-subtree token-order))))
    ;; Stalks between units.
    (dolist (token token-order)
      (when (not (zerop (cl-conllu:token-head token))) ; skip the root in merging up
        (let* ((from-graph (gethash (cl-conllu:token-head token) token-id->representation))
               (to-graph (gethash (cl-conllu:token-id token) token-id->representation))
               (connected-graph
                 (join-graphs from-graph to-graph
                              (apply #'connection-graph
                                     (append
                                      ;; the direction and stalk relation function are defined
                                      ;; globally
                                      (gethash (cl-conllu:token-deprel token)
                                               *deprel->stalk-spec*)
                                      (list from-graph to-graph))))))
          (setf from-graph connected-graph to-graph connected-graph))))
    ;; At the end, the root should contain the whole representation
    (gethash (first token-order) token-id->representation)))
