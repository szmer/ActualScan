;;;;
;;;; Building semantic representations from preprocessed text (syntactic trees).
;;;;
(declaim (optimize (debug 3)))
(in-package :omnivore)

;;;
;;; Translating from dependency relations to semantic stalks.
;;;
;;; See:
;;; https://universaldependencies.org/u/dep/all.html
;;; https://github.com/clir/clearnlp-guidelines/blob/master/md/specifications/dependency_labels.md
;;;
;;; This hashtable needs to show the (semantic) direction of the stalk and with which function to
;;; create it.
(defparameter *deprel->stalk-spec*
  (alexandria:alist-hash-table
   (list
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
    `("nsubjpass" :backward ,#'graph-obj-dangling-stalk)
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

;;;
;;; Reading functions.
;;;
(defun token->representation (input-token)
  "Determines whether we know the word contained by the token and returns either a known
representation or an unknown-token guess at it."
  (declare (type conllu.rdf::token input-token))
  (let ((official-form))
    (or official-form ; don't expand to the explanation on default
        (unknown-token->graph input-token))))

;;; For the unknown token case.
(defun unknown-token->graph (input-token)
  "Get a semantic graph that represents the input-token from a CoNNLU tree,
assuming that we have no definition for that term."
  (declare (type conllu.rdf::token input-token))
  (let ((universal-pos (cl-conllu:token-upostag input-token)))
    (apply
     (alexandria:curry #'create-graph :unknown-token)
     ;;
     ;; part is controversial here, gotta extract the possessive somehow
     ;;(if (find universal-pos '("x" "sym" "punct" "intj" "aux" "part") :test #'equalp)
     ;;
     ;; (we were previously just leaving an empty graph here, but then the parser marks random words
     ;; as eg. interjections)
     ;;
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
           ()))))))

(defun sentence-semantic-token-order (sentence)
  (let* ((tokens (cl-conllu:sentence-tokens sentence))
         (head->token-ids (make-hash-table))
         (root) (token-order))
    ;; Go through the tokens to find the root and make a mapping of tokens to children.
    (dolist (token tokens)
      (push (cl-conllu:token-id token)
            (gethash (cl-conllu:token-head token) head->token-ids))
      (when (equalp (cl-conllu:token-deprel token) "ROOT")
        (setf root token) (push root token-order)))
    ;; Order the tokens going downward from the root.
    (do ((level-token-ids (gethash (cl-conllu:token-id root) head->token-ids)
                          (reduce #'append
                                  (mapcar (lambda (token-id) (gethash token-id head->token-ids))
                                          level-token-ids))))
        ((null level-token-ids) (progn
                                 (assert (= (length token-order) (length tokens)))
                                 token-order))
      ;; Always push the new (lower) tokens to the end.
      (setf token-order (append token-order (mapcar (lambda (token-id)
                                                      ;; these token ids are one-based
                                                      (nth (1- token-id) tokens))
                                                    level-token-ids))))))
;;(mapcar #'cl-conllu:token-id (sentence-semantic-token-order (fifth *sents*)))
;;(4 8 3 22 9 7 5 2 1 23 21 20 19 11 6 24 12 10 25 16 28 17 15 27 18 14 13 26)

(defun sentence->representation (sentence)
  (let ((token-id->representation (make-hash-table))
        ;; The properly ordered list of token objects (guarantee that the children will appear
        ;; later in the tree than their parents). The first item will be the root.
        (ordered-tokens (sentence-semantic-token-order sentence)))
    ;; Set individual unit representations.
    (dolist (token ordered-tokens)
      (setf (gethash (cl-conllu:token-id token) token-id->representation)
            (token->representation token)))
    ;; Stalks between units.
    (dolist (token (reverse ordered-tokens)) ; go to the root at the end
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
                          (or (gethash (cl-conllu:token-deprel token) *deprel->stalk-spec*)
                              (error (format nil "no semantic info for relation ~A"
                                             (cl-conllu:token-deprel token))))
                          (list from-graph to-graph))))))
          ;; here we have to target gethashes intead of lexical let bindings
          (setf (gethash (cl-conllu:token-head token) token-id->representation) connected-graph
                (gethash (cl-conllu:token-id token) token-id->representation) connected-graph))))
    ;; At the end, the root should contain the whole representation
    (gethash (cl-conllu:token-id (first ordered-tokens))
             token-id->representation)))
