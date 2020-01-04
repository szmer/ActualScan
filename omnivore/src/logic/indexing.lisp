;;;;;
;;;;; Finding important traits (currently berry communities) in graphs.
;;;;;
(declaim (optimize (debug 3)))
(in-package :omnivore)

;;;
;;; TODO we could also handle *minority reports* - text themes that are rarer but important
;;;

(defun graph->list-tree (graph)
  "Get the graph's representation as a nested list, where each stalk is represented by a sublist,
whose head is the stalk label, then the destination berry label, then its outgoing stalks
represented in the same manner."
  (labels ((%berry-list-tree (berry)
                    (cons (seme-label berry)
                          (remove nil
                                  (mapcar (lambda (stalk)
                                            (when (eq (stalk-from stalk) berry)
                                              (cons (seme-label stalk)
                                                    (%berry-list-tree (stalk-to stalk)))))
                                          (berry-stalks berry))))))
    (%berry-list-tree (graph-root-berry graph))))
;; TODO consistent order (alphabetic?)

(let* ((test-graph
         (create-graph :syntax
                       (list "aaa" "bbb" "ccc" (list "ddd" :verbalp :graph-exit-p) "eee" "fff")
                       (list '(0 1) '(1 2 "pred") '(2 3) '(3 4) '(2 5))))
       (representation (graph->list-tree test-graph)))
  (pprint representation))
;;; -> ("aaa" ("-" "bbb" ("pred" "ccc" ("-" "fff") ("-" "ddd" ("-" "eee")))))
;;;
;;; Warning, not checked:
;;; (graph->list-tree (sentence->representation (fifth *sents*)))
;;; ->
;;;("_they_??;provide_them"
;;; ("obj" "something"
;;;        ("-" "_they_??;yield_them"
;;;             ("-" "??;to"
;;;                  ("-" "??;that"
;;;                       ("-" "??;of"
;;;                            ("-" "something" ("-" "something" ("-" "??;the") ("-" "Phiaton"))
;;;                                 ("-" "MS400")))))
;;;             ("pred" "??;just") ("pred" "??;only") ("subj" "??;that"))
;;;        ("-" "??;with"
;;;             ("-" "something"
;;;                  ("-" "??;for"
;;;                       ("pred" "_they_??;result_them"
;;;                               ("-" "??;in" ("-" "something" ("-" "??;isolation")))
;;;                               ("subj" "something" ("pred" "__they_be_" ("pred" "??;supraaural"))
;;;                                       ("-" "??;a") ("-" "??;headphone,"))))
;;;                  ("-" "??;the") ("-" "??;ear")))
;;;        ("pred" "__they_be_" ("pred" "??;surprisingly") ("pred" "??;good"))
;;;        ("-" "??;a") ("-" "??;coupling"))
;;; ("subj" "something" ("-" "something" ("-" "??;lambskin")) ("-" "??;the")
;;;         ("-" "??;earpad")))

(defun graph-traits (graph &key (minimum-complexity 3))
  "Return a list of string traits indentifying the graph."
  (remove nil
          (mapcar (lambda (berry)
                    (let ((subgraph (subgraph-from berry #'berry-verbalp)))
                      (when (>= (length (graph-berries subgraph))
                                minimum-complexity)
                        (format nil "~A" (graph->list-tree subgraph)))))
                  (remove-if-not #'berry-verbalp (graph-berries graph)))))

(defun index-strong-cliques (index-table &key (strength 2) (minimum-size 3))
  "Get cliques defined by sharing traits."
  (let ((included-traits) ; the ones of required length
        (strong-clique-index (make-hash-table :test #'equalp)))
    (maphash (lambda (clique-subgraph clique-sentences)
               (when (<= minimum-size (length clique-sentences))
                 (push clique-subgraph included-traits)))
             index-table)
    (alexandria:map-combinations (lambda (clique-traits)
                                   (let ((clique-elements ; intersection of clique traits
                                           (reduce (lambda (sents-1 sents-2)
                                                     (intersection sents-1 sents-2 :test #'equalp))
                                                   (mapcar (lambda (trait)
                                                             (gethash trait index-table))
                                                           clique-traits))))
                                     (when (<= 2 (length clique-elements))
                                       (setf (gethash (cl-strings:join clique-traits
                                                                       :separator ",")
                                                      strong-clique-index)
                                             clique-elements))))
                                 included-traits
                                 :length strength)
    strong-clique-index))

(defun conll-paragraph-index (conll-strings)
  "Index a list of paragraphs given as strings of ConLL-formatted sentences. The result is a list of
  paragraphs as lists of sentence with their associated traits."
  (let ((paragraph-sentence-lists (mapcar (lambda (string)
                                            (let ((string-stream (make-string-input-stream string)))
                                              ;; read sentences from the string.
                                              (cl-conllu:read-stream string-stream)))
                                          conll-strings)))
    (mapcar (lambda (paragraph)
              ;; Make lists of sentence text + its representation traits.
              (mapcar (lambda (sentence-obj)
                        (cons (cl-conllu:sentence->text sentence-obj)
                              (graph-traits (sentence->representation sentence-obj))))
                      paragraph))
            paragraph-sentence-lists)))

(defun conll-file-paragraphs-index (conll-path)
  "Index a file to a list of paragraphs associated with their traits."
  (conll-paragraph-index
   (do* ((conll-file (open conll-path :if-does-not-exist nil))
         (line (when conll-file (read-line conll-file nil))
               (read-line conll-file nil))
         (paragraph-strings))
        ((not line)
         (mapcar (lambda (lines) (cl-strings:join (reverse lines)
                                                  :separator (coerce '(#\Newline) 'string)))
                 paragraph-strings))
     (if (equalp line "#####")
         (push nil paragraph-strings)
         (push line (first paragraph-strings))))))

(defun paragraphs->trait-cliques (paragraphs)
  (let ((trait-index (make-hash-table :test #'equalp)))
    (dolist (paragraph paragraphs)
      (dolist (sentence-entry paragraph)
        (dolist (trait (rest sentence-entry))
          (push (first sentence-entry)
                (gethash trait trait-index)))))
    trait-index))

(defun paragraph-index-query (paragraphs query-string)
  "Make query on a paragraph index. Returns a trait index."
  (let ((query-index (make-hash-table :test #'equalp))
        (sentences-with-traits))
    (dolist (paragraph paragraphs)
      (when (some (lambda (sentence-entry)
                    (search query-string (first sentence-entry)))
                  paragraph)
        (dolist (sentence-entry paragraph)
          (dolist (trait (rest sentence-entry))
            (push sentence-entry
                  sentences-with-traits)
            (push (first sentence-entry)
                  (gethash trait query-index))))))
    (list query-index
          sentences-with-traits)))

(defun traits-total-score (traits trait->sentences)
  (reduce #'+ (mapcar (lambda (trait) (length (gethash trait trait->sentences)))
                      traits)))

(defun sentence-top-scores (sentences-with-traits trait-index)
  (sort (mapcar (lambda (sentence-entry)
                  (cons (first sentence-entry)
                        (traits-total-score (rest sentence-entry) trait-index)))
                sentences-with-traits)
        #'> :key #'cdr))

(defun paragraphs-sentence-top-scores (paragraphs trait-index)
  ;; takes about a minute on the test corpus
  (sentence-top-scores (reduce #'append paragraphs) trait-index))

(defun query-sentence-top-scores (query-list)
  ;; Give the sentence list and the trait index.
  (sentence-top-scores (second query-list) (first query-list)))

;;;;
;;;; Printing and diagnostics.
;;;;
(defun write-trait-sizes-csv (index-table path)
  (with-open-file (lengths-file path :direction :output :if-does-not-exist :create
                                     :if-exists :supersede)
    (maphash (lambda (trait sents)
               (format lengths-file "\"~A\" ~A~%" (csv-sanitized-string trait) (length sents)))
             index-table)))

(defun index-summarize-cliques (index-table &key (minimum-size 3))
  (maphash (lambda (clique-subgraph clique-sentences)
             (when (<= minimum-size (length clique-sentences))
               (format t "~A ~A ~A~%"
                       clique-subgraph
                       ;; the shortest utterance.
                       (first (sort (copy-list clique-sentences) #'< :key #'length))
                       (length clique-sentences))))
           index-table))

(defun index-pprint-cliques (index-table &key (minimum-size 3) (max-printed 7) required-atom)
  (maphash (lambda (clique-subgraph clique-sentences)
             (let ((clique-sentences ; KLUDGE KLUDGE should be done upstream!
                     (remove-duplicates clique-sentences
                                        :test #'equalp
                                        :key (lambda (sentence)
                                               (if (search "Click to expand..." sentence)
                                                   (subseq sentence 0 (- (length sentence) 18))
                                                   sentence)))))
               (when (and (<= minimum-size (length clique-sentences))
                          (if (not required-atom) t
                              (search required-atom clique-subgraph)))
                 (let ((sorted-clique (sort (copy-list clique-sentences) #'< :key #'length)))
                   (format t "~%####~A ~A~%" clique-subgraph (length clique-sentences))
                   (block printing
                     (do ((counter 0 (1+ counter))
                          (sentence (pop sorted-clique) (pop sorted-clique)))
                         ((or (= counter max-printed) (null sentence)))
                       (format t "~A~%" sentence)))))))
           index-table))
