(declaim (optimize (debug 3)))

(defpackage :cl-lectrix (:use :cl :trivial-types))
(in-package :cl-lectrix)

(defun truep (arg) (not (not arg)))

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

;;;(defparameter *a* (create-graph :syntax (list "something" "nothing") (list '(1 0))))
;;;(defparameter *b* (token-tree->representation (cl-conllu:sentence-binary-tree (fifth *sents*))))

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
  ((verbalp :reader berry-verbalp :initarg :verbalp :initform nil :type boolean)
   ;; important if this is a verbal, whether it takes an obj exit:
   (obj-exit-p :reader berry-obj-exit-p :initarg :obj-exit-p :initform nil :type boolean)
   (stalks :accessor berry-stalks :initform nil :type list)
   ;;
   ;; Valence correction slots.
   (valence-instructions :reader berry-valence-instructions :initarg :valence-instructions
                         :initform nil :type list)
   ;; this is set to t after valence correcting is ran, which should execute all instructions
   ;; from the appropriate slot
   (valence-corrected :accessor berry-valence-corrected :initform nil :type boolean)))
(defmethod print-object ((obj berry) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a/~a" (seme-label obj) (seme-creator obj))))

(defclass stalk (seme)
    ((weight :reader stalk-weight :initarg :weight :type real)
     (from :accessor stalk-from :initarg :from :type berry)
     (to :accessor stalk-to :initarg :to :type berry)))
(defmethod print-object ((obj stalk) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a->~a"
            (seme-label (stalk-from obj))
            (seme-label (stalk-to obj)))))

(defun stalk-not-registered-p (prospective-stalk)
  "Check that the berry led to has no existing stalk from the origin berry, and vice versa."
  ;; We have to check both stalk lists to prevent pushing the stalk if it appears on one
  ;; of them.
  (and (not (find-if (lambda (stalk) (eq (stalk-from prospective-stalk)
                                         (stalk-from stalk)))
                     (berry-stalks (stalk-to prospective-stalk))))
       (not (find-if (lambda (stalk) (eq (stalk-to prospective-stalk)
                                         (stalk-to stalk)))
                     (berry-stalks (stalk-from prospective-stalk))))))

(defun register-stalk (prospective-stalk)
  "Ensure that the stalk is added to the adjacency lists of its berries. We raise a recoverable
error if a stalk between the berries already exists. Returns the stalk."
  (if (stalk-not-registered-p prospective-stalk)
      (progn
        (push prospective-stalk (berry-stalks (stalk-from prospective-stalk)))
        (push prospective-stalk (berry-stalks (stalk-to prospective-stalk)))
        prospective-stalk)
      (cerror "a stalk already exists between berries" prospective-stalk)))

(defun connect-with-stalk (berry1 berry2 creator &key (label ""))
  "Connects both nodes with a fresh stalk and adjoins it to their adjacency lists. The stalk is
  returned. We raise a recoverable error if a stalk between the berries already exists."
  (let ((stalk (make-instance 'stalk :from berry1 :to berry2 :label label
                                     :creator creator
                                     :weight (gethash creator *creator->stalk-weight*))))
    (register-stalk stalk)))

(defun stalk-connect (direction stalk target-berry)
  (case direction
    (:from (setf (stalk-from stalk) target-berry) stalk)
    (:to (setf (stalk-to stalk) target-berry) stalk))
  (register-stalk stalk))

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
;; TODO handle the case where there are many
(defmethod print-object ((obj graph) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~%berries:~a~%stalks:~a~%" (graph-berries obj) (graph-stalks obj))))

(defun create-graph (creator berry-specs stalk-specs)
  "Intended for new graphs. The creator applies for all created semes, and also for determining edge
weights. Berries are defined by labels (optionally in a list with :verbalp and :obj-exit-p) and
stalks by berry indices."
  (let ((berries (mapcar
                  (lambda (berry-spec)
                    (let ((berry-spec (if (listp berry-spec) berry-spec
                                          (list berry-spec))))
                      (make-instance 'berry
                                     :label (first berry-spec)
                                     :creator creator
                                     :verbalp (truep (find :verbalp berry-spec))
                                     :obj-exit-p (truep (find :obj-exit-p berry-spec)))))
                  berry-specs)))
    (make-instance 'graph
                   :berries berries
                   :stalks (mapcar
                            (lambda (stalk-spec)
                              (connect-with-stalk (nth (first stalk-spec) berries)
                                                  (nth (second stalk-spec) berries)
                                                  creator
                                                  :label (or (third stalk-spec) "")))
                            stalk-specs))))

(defun concatenate-graphs (&rest graphs)
  "Return a graph object concatenating graphs' berry and stalk lists. No actual connection check is
performed. The first graph's root becomes the root of the result."
  (make-instance 'graph
                 :berries (reduce #'append (mapcar #'graph-berries graphs))
                 :stalks (reduce #'append (mapcar #'graph-stalks graphs))))

(defun nearest-from (start-berry test-function &key (backwards-to-root-p nil))
  "Return the nearest berry that satisties the test-function. Unless backwards-to-root-p, we move
 only with stalks away from the root (or towards it otherwise). Return nil if no such berry can be
 find. The exact berry returned is undefined if there are multiple nearest ones that satisfy the
 test-function."
  (do* ((stalk-forward-function (if backwards-to-root-p #'stalk-to #'stalk-from))
        ;; Paths = individual stalks leading us "forward". We don't need to store trails, since we
        ;; move in one direction in a graph guaranteed to accomodate for that.
        (paths
         ;; remove stalks that would move us backwards.
         (remove-if (lambda (stalk) (eq (funcall stalk-forward-function stalk)
                                        start-berry))
                    (berry-stalks start-berry)))
        (current-path (pop paths) (pop paths)))
       ((not current-path) (values nil nil)) ; the not found case
    ;; If we are sure that a current-path exists, we can extract the next berry it leads to.
    (let* ((next-berry (funcall stalk-forward-function current-path)))
      (if (funcall test-function next-berry)
          ;; The success return:
          (return-from nearest-from next-berry)
          (setf paths
                ;; Append new paths leading from the next-berry at the end, to ensure a
                ;; breadth-first search.
                (append paths
                        ;; remove stalks that would move us backwards.
                        (remove-if (lambda (stalk) (eq (funcall stalk-forward-function
                                                                stalk)
                                                       next-berry))
                                   (berry-stalks next-berry))))))))

;;
;; Main semantic "exits" of the structures.
;;
;; NOTE all these follow a simple-minded approach, that can be then corrected with valence
;; instructions.
;;

(defmacro define-graph-stalk-function (exit-name &key stalk-label)
  "Provided that the graph-(exit-name) function exists, define a function
graph-(exit-name)-dangling-stalk returning a dangling stalk to/from the exit in question with
(creator direction graph) arguments, which, in case the important berry is a verbal, is properly
labeled - with the exit-name by default."
  (let ((exit-function-symbol
          (read-from-string (concatenate 'string "graph-" exit-name "-berry"))))
    `(defun
         ;; assemble the function name
         ,(read-from-string (concatenate 'string "graph-" exit-name "-dangling-stalk"))
         ;; the arguments:
         (creator direction graph)
       "Return a dangling stalk from/to the graph's exit. It should be completed with the
connect-stalk function."
       (let ((important-berry (,exit-function-symbol graph)))
         (if (berry-verbalp important-berry)
             ;; (note that we use the direction as source of the appropriate keyword argument)
             (make-instance 'stalk :creator creator (the keyword direction) important-berry)
             (make-instance 'stalk :creator creator (the keyword direction) important-berry
                            :label ,(or stalk-label exit-name)))))))

(defun graph-root-berry (graph) (first (graph-berries graph)))
(define-graph-stalk-function "root" :stalk-label "pred")

(defun graph-subj-berry (graph)
  (if (or (berry-verbalp (graph-root-berry graph))
          (equalp "something" (seme-label (graph-root-berry graph))))
      (graph-root-berry graph)
      (error "don't know how to extract subj from the graph")))
(define-graph-stalk-function "subj")

(defun graph-obj-berry (graph)
  (if (berry-verbalp (graph-root-berry graph))
      (graph-root-berry graph)
      ;; Technically it can be a *something* plugged into some verbal, but the verbal must be
      ;; equivalent to it per our graph representation semantics.
      ;; TODO alternatively search for this something with a good test predicate?
      (or (nearest-from (graph-root-berry graph) #'berry-verbalp)
          (error "don't know how to extract obj from the graph"))))
(define-graph-stalk-function "obj")

(defun graph-pred-berry (graph)
  (if (berry-verbalp (graph-root-berry graph))
      (graph-root-berry graph)
      (or (nearest-from (graph-root-berry graph) #'berry-verbalp)
          (error "don't know how to extract pred from the graph"))))
(define-graph-stalk-function "pred")

(defun graph-sit-berry (graph)
  (if (berry-verbalp (graph-root-berry graph))
      (graph-root-berry graph)
      (or (nearest-from (graph-root-berry graph) #'berry-verbalp)
          (error "don't know how to extract sit from the graph"))))
(define-graph-stalk-function "sit")

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
         (cond
           ((equalp "verb" universal-pos)
            (list
             (list
              (list (cl-strings:join
                     (list "_they_" *prefix-unknown-token* (cl-conllu:token-lemma input-token)
                           "_them"))
                    :verbalp :obj-exit-p))
             ()))
           ((equalp "noun" universal-pos)
            (list
             (list "something"
                   (cl-strings:join (list
                                     *prefix-unknown-token*
                                     (cl-conllu:token-lemma input-token))))
             '((0 1))))
           ((equalp "propn" universal-pos)
            (list
             (list "something"
                   ;; A proper noun doesn't have its label marked as unknown semantic
                   ;; component, we assume it to be a unique identifier.
                   (cl-conllu:token-lemma input-token))
             '((0 1))))
           ((equalp "adj" universal-pos)
            (list
             (list "__they_be_"
                   (cl-strings:join (list *prefix-unknown-token*
                                          (cl-conllu:token-lemma input-token))))
             '((0 1))))
           (t (list
               (list (cl-strings:join
                      (list *prefix-unknown-token*
                            (cl-conllu:token-lemma input-token))))
               ())))))))

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
    ;; NOTE we currently ignore the direction here when constructing the graph, to preserve always
    ;; stalking from the root outward. (All backwards relations connect to root to avoid exits
    ;; unavailable in graph children)
    ;;
    ;; NOTE that cdrs are the values in alists, so we get lists here by default (somewhat
    ;; confusingly) also we need to unquote functions to make them functions, not (function ...)
    ;; lists rejected by the compiler (!)
    `("acl" :forward ,#'graph-obj-dangling-stalk) ; clausal modifier tends to be an obj, but we need to consult surroundings in postpro
    `("advcl" :backward ,#'graph-sit-dangling-stalk) ; at least judging by UD's examples
    `("advmod" :backward ,#'graph-pred-dangling-stalk)
    `("agent" :forward ,#'graph-root-dangling-stalk) ; most often "by"?
    `("acomp" :forward ,#'graph-root-dangling-stalk)
    `("amod" :forward ,#'graph-root-dangling-stalk)
    ;; NOTE probably should be proxied by a they_be_them
    `("appos" :forward ,#'graph-root-dangling-stalk)
    `("attr" :forward ,#'graph-root-dangling-stalk)
    `("aux" :backward ,#'graph-pred-dangling-stalk)
    `("auxpass" :backward ,#'graph-pred-dangling-stalk)
    ;; NOTE coordinating conjunction, should proxy the conj relation in question (postprocessing)
    `("cc" :forward ,#'graph-root-dangling-stalk)
    `("conj" :forward ,#'graph-root-dangling-stalk)
    `("ccomp" :backward ,#'graph-obj-dangling-stalk)
    `("compound" :forward ,#'graph-root-dangling-stalk)
    `("det" :forward ,#'graph-root-dangling-stalk)
    `("dobj" :backward ,#'graph-obj-dangling-stalk)
    `("mark" :forward ,#'graph-root-dangling-stalk) ; ??
    `("neg" :forward ,#'graph-root-dangling-stalk)
    `("npadvmod" :forward ,#'graph-root-dangling-stalk) ; no legitimate case seen
    `("npmod" :forward ,#'graph-root-dangling-stalk) ; no legitimate case seen
    `("nsubj" :backward ,#'graph-subj-dangling-stalk)
    `("nsubj" :backward ,#'graph-subj-dangling-stalk) ; semantically passive, dubious
    `("pcomp" :forward ,#'graph-root-dangling-stalk)
    `("pobj" :forward ,#'graph-root-dangling-stalk) ; can also lead to verbals
    `("poss" :forward ,#'graph-root-dangling-stalk)
    `("prt" :forward ,#'graph-root-dangling-stalk) ; particle verbs, dubious
    `("prep" :forward ,#'graph-root-dangling-stalk)
    `("punct" :forward ,#'graph-root-dangling-stalk) ; dead in practice
    `("relcl" :backward ,#'graph-subj-dangling-stalk)
    `("subtok" :backward ,#'graph-subj-dangling-stalk)
    ;; it`s possible that we want to plug into sit in verbals? NOTE also through __quote?
    `("xcomp" :backward ,#'graph-obj-dangling-stalk)
    )
   :test #'equal))

;; If the stalk is between two verbal berries, we need to ensure the proper semantic direction
;; with a proper intermediate `something` berry. Otherwise, we ignore the issue ???? (for now).
(defun connection-graph (semantic-direction stalk-fun from-graph to-graph)
  "Returns a graph necessary to connect the two graphs."
  (declare (type keyword semantic-direction) (type graph from-graph to-graph))
  (let* ((direction :to) ; possibly KLUDGE, see comment to *deprel->stalk-spec*
         (main-stalk (funcall (if (eq semantic-direction :backward)
                                  #'graph-root-dangling-stalk
                                  stalk-fun)
                              :syntax direction to-graph)))
    (if (and (berry-verbalp (graph-root-berry from-graph))
             (berry-verbalp (graph-root-berry to-graph)))
        (let ((proxy-berry (make-instance 'berry :label "something" :creator :syntax))
              ;; KLUDGE maybe this should be an unlabeled stalk.
              (proxy-stalk (graph-pred-dangling-stalk :syntax (opposite direction) from-graph)))
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
                                 (graph-root-berry from-graph)))))))

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
                 (concatenate-graphs
                  from-graph to-graph
                  (apply #'connection-graph
                         (append
                          ;; the direction and stalk relation function are defined
                          ;; globally
                          (gethash (cl-conllu:token-deprel token) *deprel->stalk-spec*)
                          (list from-graph to-graph))))))
          (setf from-graph connected-graph to-graph connected-graph))))
    ;; At the end, the root should contain the whole representation
    (gethash (cl-conllu:token-id (first token-order)) token-id->representation)))
