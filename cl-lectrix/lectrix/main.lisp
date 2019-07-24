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

(deftype element-creator () '(member
                             :explication-definition
                             :explication-prototype
                             :unknown-token
                             :syntax))
(defparameter *creator->stalk-weight*
  (list '(:explication-definition 1.0)
        '(:explication-prototype 0.8)
        '(:unknown-token 0.55)
        '(:syntax 0.5)))

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

;;
;; We represent a graph as a list of two lists: of berries and of stalks.
;; By convention, the first berry in the berry list is the root.
;;

(defparameter *prefix-unknown-token* "??;")
(defparameter *prefix-proper-name-token* "==;")

(defclass graph ()
  ((berries :reader graph-berries :initarg :berries :type (proper-list berry))
   (stalks :reader graph-stalks :initarg :stalks :type (proper-list stalks))))

(defgeneric root (graph))
(defmethod root ((graph graph)) (first (graph-berries graph)))

;;
;; Main semantic "exits" of the structures.
;;

;; NOTE The root from a verbal berry defaults to pred.

(defun subj-stalk (graph direction)
  "Returns a stalk that can be used to plug into the semantic
structure's sub (indicated in the :to property)."
  (declare (type (graph direction graph direction)))
  nil)

(defun obj-stalk (graph direction)
  nil)

(defun pred-stalk (graph direction)
  nil)

(defun sit-stalk (graph direction)
  nil)

;;
;; Translation from CoNLL.
;;

(defun unknown-token->graph (input-token)
  "Get a semantic graph that represents the input-token from a CoNNLU tree,
assuming that we have no definition for that term."
  (declare (type input-token conllu.rdf::token))
  (let* ((universal-pos (candies:string->symbol (cl-conllu:token-upostag input-token)))))
  (if (find universal-pos '(x sym punct intj aux part)) ; part is controversial, gotta extract the possessive somehow
      (case universal-pos
        (verb (list
               (list (make-instance
                      'berry
                      :label (candies:join-strings "_they_" *prefix-unknown-token*
                                                   (cl-conllu:token-lemma input-token)
                                                   "_them")
                      :verbalp t :obj-exit-p t :element-creator :unknown-token))
               ()))
        (noun (let ((berries (list (make-instance 'berry :label "something"
                                                         :element-creator :unknown-token)
                                   (make-instance 'berry :label (candies:join-strings
                                                                 *prefix-unknown-token*
                                                                 (cl-conllu:token-lemma input-token))
                                                         :element-creator :unknown-token))))
                (list berries
                      (list (make-instance 'stalk :from (nth 0 berries) :to (nth 1 berries)
                                                  :weight (cdr (assoc :syntax *creator->stalk-weight*))
                                                  :element-creator :unknown-token)))))
        (propn (let ((berries (list (make-instance 'berry :label "something"
                                                          :element-creator :unknown-token)
                                    ;; A proper noun doesn't have its label marked as unknown semantic
                                    ;; component, we assume it to be a unique identifier.
                                    (make-instance 'berry :label (cl-conllu:token-lemma input-token)
                                                          :element-creator :unknown-token))))
                 (list berries
                       (list (make-instance 'stalk :from (nth 0 berries) :to (nth 1 berries)
                                                   :weight (cdr (assoc :syntax *creator->stalk-weight*))
                                                   :element-creator :unknown-token)))))
        (adj (let ((berries (list (make-instance 'berry :label "__they_be_"
                                                        :verbalp t
                                                        :element-creator :unknown-token)
                                  (make-instance 'berry :label (candies:join-strings
                                                                *prefix-unknown-token*
                                                                (cl-conllu:token-lemma input-token))
                                                        :element-creator :unknown-token))))
               (list berries
                     (list (make-instance 'stalk :from (nth 0 berries) :to (nth 1 berries)
                                                 :weight (cdr (assoc :syntax *creator->stalk-weight*))
                                                 :element-creator :unknown-token)))))
        (otherwise
         (list (list (make-instance 'berry :label (candies:join-strings
                                                   *prefix-unknown-token*
                                                   (cl-conllu:token-lemma input-token))
                                           :element-creator :unknown-token))
               (list (make-instance 'stalk :from (nth 0 berries) :to (nth 1 berries)
                                           :weight (cdr (assoc :syntax *creator->stalk-weight*))
                                           :element-creator :unknown-token)))))
      ;; for a null representation:
      (list nil nil)))

(defun token->representation (input-token)
  "You get a list of two: a list of berries and a list of stalks."
  (declare (type input-token conllu.rdf::token))
  (let ((explication))
    (or explication
        ;; one berry, no stalks:
        (unknown-token->graph input-token))))

;;
;; Translating from dependency relations to semantic stalks.
;;

;; See:
;; https://universaldependencies.org/u/dep/all.html
;; https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md
;;
;; This hashtable needs to show the (semantic) direction of the stalk and with which function to
;; create it. If the stalk is between two verbal berries, we need to ensure the proper semantic direction
;; with a proper intermediate `something' berry. Otherwise, we ignore the issue ???? (for now).
(defparameter *deprel->stalk-spec*
  (alexandria:alist-hash-table
   (list
    '("acl" '(:forward #'designated-obj)) ; clausal modifier tends to be an obj, but we need to consult surroundings in postpro
    '("advcl" '(:backward #'designated-sit)) ; at least judging by UD's examples
    '("advmod" '(:backward #'designated-pred))
    '("agent" '(:forward #'root)) ; most often "by"?
    '("acomp" '(:forward #'root))
    '("amod" '(:forward #'root))
    ;; NOTE probably should be proxied by a they_be_them
    '("appos" '(:forward #'root))
    '("attr" '(:forward #'root))
    '("aux" '(:backward #'designated-pred))
    '("auxpass" '(:backward #'designated-pred))
    ;; NOTE coordinating conjunction, should proxy the conj relation in question (postprocessing))
    '("cc" '(:forward #'root))
    '("conj" '(:forward #'root))
    '("ccomp" '(:backward #'designated-obj))
    '("compound" '(:forward #'root))
    '("det" '(:forward #'root))
    '("dobj" '(:backward #'designated-obj))
    '("mark" '(:forward #'root)) ; ??
    '("neg" '(:forward #'root))
    '("npadvmod" '(:forward #'root)) ; no legitimate case seen
    '("npmod" '(:forward #'root)) ; no legitimate case seen
    '("nsubj" '(:backward #'designated-subj))
    '("nsubj" '(:backward #'designated-subj)) ; semantically passive, dubious
    '("pcomp" '(:forward #'root))
    '("pobj" '(:forward #'root)) ; can also lead to verbals
    '("poss" '(:forward #'root))
    '("prt" '(:forward #'root)) ; particle verbs, dubious
    '("prep" '(:forward #'root))
    '("punct" '(:forward #'root)) ; dead in practice
    '("relcl" '(:backward #'designated-subj))
    '("subtok" '(:backward #'designated-subj))
    ;; it's possible that we want to plug into sit in verbals? NOTE also through __quote?
    '("xcomp" '(:backward #'designated-obj))
    )
   :test #'equal))

;; See the comment above on *deprel->stalk-spec*.
(defun directed-connection (direction stalk-fun graph1 graph2)
  "Returns semantic graph lists necessary to connect the two graphs."
  (declare (type keyword direction) (type (function stalk-fun)))
  (when (and (berry-verbalp (root graph1)) (berry-verbalp (root graph2)))
    (let* ((proxy-berry (make-instance 'berry :label "something" :creator :syntax))
           (empty-stalk (make-instance 'stalk :to proxy-berry :label "pred"))
           (main-stalk (funcall stalk-fun (ccase direction (:forward graph2)
                                                 (:backward graph1)))))
      (setf (stalk-from main-stalk) proxy-berry)
      (setf (stalk-from empty-stalk)
            (root (ccase direction
                    (:forward graph1)
                    (:backward graph2))))
      (list (list proxy-berry)
            (list empty-stalk main-stalk)))
    ;; The simple case, no verbals.
    (list '() (list stalk))))

(defun token-tree->representation (tree)
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
             (token->representation subtree))
       (push subtree token-order))))
  ;; Stalks between units.
  (dolist (token token-order)
    (when (not (zerop (cl-conllu:token-head token))) ; skip the root, obviously
      (let ((connection-graph
              (apply #'directed-connection
                     (append
                      ;; The direction and stalk-producing function.
                      (gethash (cl-conllu:token-deprel token) *deprel->stalk-spec*)
                      ;; From the head to this token.
                      (list (gethash (cl-conllu:token-head token) token-id->representation)
                            (gethash (cl-conllu:token-id token) token-id->representation)))))))
      (rplacd (graph-berries result-graph) (cons (graph-berries connection-graph)
                                                 (cdr (graph-berries result-graph))))
      (setf (graph-stalks result-graph) (cons (graph-stalks connection-graph)
                                              (graph-stalks result-graph)))))
  ;; Collapse all representations.
  (maphash (lambda (id representation)
             (declare (ignore id))
             (setf (graph-berries result-graph) ; berries
                   (cons (graph-berries representation) (graph-berries result-graph)))
             (setf (graph-stalks result-graph) ; stalks
                   (cons (graph-stalks representation) (graph-stalks result-graph))))
           token-id->representation)
  result-graph)
