;;;;
;;;; Building semantic representations from preprocessed text (syntactic trees).
;;;;
;;;;-(declaim (optimize (debug 3)))
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
    ;;
    ;; observed on spaces (_/SPACE), should be removed from semantics probably
    `("" :backward ,#'graph-root-dangling-stalk)
    ;; ACL Clausal modifier of noun: sites -> offering (... booking facilities)
    `("acl" :forward ,#'graph-obj-dangling-stalk) ; clausal modifier tends to be an obj, but we need
                                        ; to consult surroundings in postpro
    ;; ADVCL Adverbial clause modifier: (he was) upset -> (when I) talked (to him)
    `("advcl" :backward ,#'graph-sit-dangling-stalk) ; at least judging by UD's examples
    ;; ADVMOD Adverbial modifier: less <- often
    `("advmod" :backward ,#'graph-root-dangling-stalk)
    `("agent" :forward ,#'graph-root-dangling-stalk) ; most often "by"?
    `("acomp" :forward ,#'graph-root-dangling-stalk)
    `("amod" :forward ,#'graph-root-dangling-stalk)
    ;; NOTE probably should be proxied by a _sth_is_
    `("appos" :forward ,#'graph-root-dangling-stalk)
    `("attr" :forward ,#'graph-root-dangling-stalk)
    `("aux" :backward ,#'graph-pred-dangling-stalk)
    `("auxpass" :backward ,#'graph-pred-dangling-stalk)
    ;; a case marking, its role should be grammatical and so should modify verbal connections?
    `("case" :backward ,#'graph-root-dangling-stalk)
    ;; NOTE coordinating conjunction, should proxy the conj relation in question (postprocessing)
    `("cc" :forward ,#'graph-root-dangling-stalk)
    `("conj" :forward ,#'graph-root-dangling-stalk)
    `("ccomp" :backward ,#'graph-obj-dangling-stalk)
    `("compound" :forward ,#'graph-root-dangling-stalk)
    ;; these are clauses residing in the subject position of either an active or passive verb.
    `("csubj" :backward ,#'graph-subj-dangling-stalk)
    `("csubjpass" :backward ,#'graph-subj-dangling-stalk)
    `("dative" :backward ,#'graph-pred-dangling-stalk) ; "cost->me $5"
    `("det" :forward ,#'graph-root-dangling-stalk)
    `("dep" :backward ,#'graph-sit-dangling-stalk) ; an "unclassified dependent", great
    `("dobj" :backward ,#'graph-obj-dangling-stalk)
    `("expl" :backward ,#'graph-root-dangling-stalk) ; expletive, should be purely grammatical
    `("intj" :backward ,#'graph-root-dangling-stalk)
    `("mark" :forward ,#'graph-root-dangling-stalk) ; ??
    `("meta" :backward ,#'graph-root-dangling-stalk)
    `("neg" :forward ,#'graph-root-dangling-stalk)
    `("npadvmod" :forward ,#'graph-root-dangling-stalk) ; no legitimate case seen
    `("npmod" :forward ,#'graph-root-dangling-stalk) ; no legitimate case seen
    `("nmod" :backward ,#'graph-root-dangling-stalk)
    `("nsubj" :backward ,#'graph-subj-dangling-stalk) ; semantically passive, dubious
    `("nsubjpass" :backward ,#'graph-obj-dangling-stalk)
    `("nummod" :backward ,#'graph-root-dangling-stalk)
    `("oprd" :backward ,#'graph-pred-dangling-stalk) ; object predicate, as in "considered->portable"
    ;; this is supposed to give side parenthetical information on the head, but can be "are" in
    ;; "they are laid back" (laid being the root of the tree according to the parser)
    `("parataxis" :backward ,#'graph-root-dangling-stalk)
    `("pcomp" :forward ,#'graph-root-dangling-stalk)
    `("pobj" :forward ,#'graph-root-dangling-stalk) ; can also lead to verbals
    `("poss" :forward ,#'graph-root-dangling-stalk)
    ;; this seems to be something that appears before a determiner, like in "all <-[the<-]- ones"
    `("predet" :backward ,#'graph-root-dangling-stalk) ; NOT in glossary!
    `("prt" :forward ,#'graph-root-dangling-stalk) ; particle verbs, dubious
    `("prep" :forward ,#'graph-root-dangling-stalk)
    `("possessive" :forward ,#'graph-root-dangling-stalk) ; probably the kind of word should be caught on token level
    ;; first part of conjunction ("both", "neither"), dependent on the first conjunct, probably
    ;; should be shifted to the conjuncting word in postprocessing
    `("preconj" :backward ,#'graph-root-dangling-stalk)
    `("punct" :forward ,#'graph-root-dangling-stalk) ; dead in practice
    `("quantmod" :backward ,#'graph-root-dangling-stalk)
    `("relcl" :backward ,#'graph-subj-dangling-stalk)
    `("subtok" :backward ,#'graph-root-dangling-stalk) ; NOT in glossary!
    `("vocative" :backward ,#'graph-root-dangling-stalk)
    ;; it`s possible that we want to plug into sit in verbals? NOTE also through __quote?
    `("xcomp" :backward ,#'graph-obj-dangling-stalk)
    )
   :test #'equal))

;;; This is just yanked from SpaCy glossary for reference.
(defparameter *deprel-glosary*
  (alexandria:alist-hash-table
   (list
    '("acl" "clausal modifier of noun (adjectival clause)")
    '("acomp" "adjectival complement")
    '("advcl" "adverbial clause modifier")
    '("advmod" "adverbial modifier")
    '("agent" "agent")
    '("amod" "adjectival modifier")
    '("appos" "appositional modifier")
    '("attr" "attribute")
    '("aux" "auxiliary")
    '("auxpass" "auxiliary (passive)")
    '("case" "case marking")
    '("cc" "coordinating conjunction")
    '("ccomp" "clausal complement")
    '("clf" "classifier")
    '("complm" "complementizer")
    '("compound" "compound")
    '("conj" "conjunct")
    '("cop" "copula")
    '("csubj" "clausal subject")
    '("csubjpass" "clausal subject (passive)")
    '("dative" "dative")
    '("dep" "unclassified dependent")
    '("det" "determiner")
    '("discourse" "discourse element")
    '("dislocated" "dislocated elements")
    '("dobj" "direct object")
    '("expl" "expletive")
    '("fixed" "fixed multiword expression")
    '("flat" "flat multiword expression")
    '("goeswith" "goes with")
    '("hmod" "modifier in hyphenation")
    '("hyph" "hyphen")
    '("infmod" "infinitival modifier")
    '("intj" "interjection")
    '("iobj" "indirect object")
    '("list" "list")
    '("mark" "marker")
    '("meta" "meta modifier")
    '("neg" "negation modifier")
    '("nmod" "modifier of nominal")
    '("nn" "noun compound modifier")
    '("npadvmod" "noun phrase as adverbial modifier")
    '("nsubj" "nominal subject")
    '("nsubjpass" "nominal subject (passive)")
    '("nounmod" "modifier of nominal")
    '("npmod" "noun phrase as adverbial modifier")
    '("num" "number modifier")
    '("number" "number compound modifier")
    '("nummod" "numeric modifier")
    '("oprd" "object predicate")
    '("obj" "object")
    '("obl" "oblique nominal")
    '("orphan" "orphan")
    '("parataxis" "parataxis")
    '("partmod" "participal modifier")
    '("pcomp" "complement of preposition")
    '("pobj" "object of preposition")
    '("poss" "possession modifier")
    '("possessive" "possessive modifier")
    '("preconj" "pre-correlative conjunction")
    '("prep" "prepositional modifier")
    '("prt" "particle")
    '("punct" "punctuation")
    '("quantmod" "modifier of quantifier")
    '("rcmod" "relative clause modifier")
    '("relcl" "relative clause modifier")
    '("reparandum" "overridden disfluency")
    '("root" "root")
    '("vocative" "vocative")
    '("xcomp" "open clausal complement"))
   :test #'equal))

;;; Temporary 'fake' dictionary, serving only for associating the "people" berry with some lemmas.
;;;-(defparameter *temp-dictionary*
;;;-  (alexandria:alist-hash-table
;;;-   (mapcar (lambda (term)
;;;-             (cons term
;;;-                   `(create-graph :explication-definition (list ,term "people") '((0 1)))))
;;;-           ;; We skip "headband" because its semantic relation to people is probably indirect.
;;;-           ;; similarly technoical terms like "frequency", "gain"
;;;-           ;; a "design" is made by people, but not necessarily affecting them?
;;;-           (list "anyone" "audio" "audiophile" "bass" "bright" "can" "comfortable" "detail" "difficult"
;;;-                 "ear" "easy" "experience" "friend" "forum" "gear"
;;;-                 "hard" "harsh" "head" "headphone" "high" "hobby" "idea" "imaging"
;;;-                 "music" "musical" "mid" "midrange" "natural" "neutral" "order"
;;;-                 "pad" "person" "price" "product" "recording" "someone" "sound" "soundstage" "small"
;;;-                 "treble" "warm" "volume"))
;;;-   :test #'equalp))

;;;
;;; Reading functions.
;;;
(defun verb-canonical-form (verb-lemma &key unknownp)
  (cl-strings:join
    (list "_sth_" *prefix-unknown-token*
          (third-person-present verb-lemma) (if (equalp "be" verb-lemma)
                                              "_" "_sth"))))

(defun token->representation (input-token)
  "Determines whether we know the word contained by the token and returns either a known
representation or an unknown-token guess at it."
  (declare (type conllu.rdf::token input-token))
  ;; If an explication is available, we should a graph spec from the database.
  (let ((explication
          (or
            (unless (find (cl-conllu:token-upostag input-token)
                          (list "punct" "sym") ; avoid checking for ., /
                          :test #'equalp)
              (explication-lookup (if (equalp "verb" (cl-conllu:token-upostag input-token))
                                      (verb-canonical-form (cl-conllu:token-lemma input-token))
                                      (cl-conllu:token-lemma input-token))))
            ;; this is a KLUDGE to be removed when possible:
            (eval ; note that (eval nil) is ok, gives us nil
             (gethash (cl-conllu:token-lemma input-token)
                              *temp-dictionary*)))))
    (or explication
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
          (list (verb-canonical-form (cl-conllu:token-lemma input-token) :unknownp t)
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
         (list '("_sth_is_" :verbalp :obj-exit-p)
               (cl-strings:join (list *prefix-unknown-token*
                                      (cl-conllu:token-lemma input-token))))
         '((0 1 "pred")))) ; TODO failure to label to-verbal stalk should be catched by validation
       (t (list
           (list (cl-strings:join
                  (list *prefix-unknown-token*
                        (cl-conllu:token-lemma input-token))))
           ()))))))

(defun sentence-semantic-token-order (sentence)
  "The properly ordered list of token objects (guarantee that the children will appear later in \
   the tree than their parents). The first item will be the root."
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

(defun sentence->representation (sentence &key debug-info)
  (let ((token-id->representation (make-hash-table))
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
                          (list from-graph to-graph)
                          (when debug-info (list :debug-dependency-label
                                                 (cl-conllu:token-deprel token))))))))
          ;; here we have to target gethashes intead of lexical let bindings
          (setf (gethash (cl-conllu:token-head token) token-id->representation) connected-graph
                (gethash (cl-conllu:token-id token) token-id->representation) connected-graph))))
    ;; At the end, the root should contain the whole representation
    (gethash (cl-conllu:token-id (first ordered-tokens))
             token-id->representation)))

(defun marker-presence (marker-name lemmas)
  "Get marker's presence indication, in the range of roughly (0,1). Zero is possible for\
   oversaturation only, as there is some positive value on zero occurrences."
  (labels ((%count-occurrences (word-list)
             (reduce #'+ (mapcar
                           (lambda (lemma) (if (find lemma word-list :test #'equalp)
                                             1 0))
                           lemmas))))
    (let ((marker-count (cond ((equalp marker-name "USAGE")
                               (%count-occurrences
                                 '("use" "using" "listen" "listened" "listening" "easy" "experience" "experienced"
                                   "enjoy" "enjoyed" "enjoying" "issue" "issues" "try" "tried" "trying"
                                   "comfortably" "situation" "situations")))
                              (t (error (format nil "Unknown marker ~A" marker-name))))))
      ;; Mean 3 and standard deviation 2 will get max on 3 (~0.2) with little (~0.06) on 0 and 6.
      (* 4
        (gaussian
          marker-count
          ;; expected mean
          (max 4 ; don't encourage short sentences
               (/ (length lemmas) 3))
          2)))))
