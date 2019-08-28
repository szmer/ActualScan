(in-package :omnivore)

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

(defun graph-indices (graph)
  "Return a list of string indices indentifying the graph."
  (mapcar (lambda (berry)
            (graph->list-tree (format nil "~A" (subgraph-from berry #'berry-verbalp))))
          (remove-if-not #'berry-verbalp (graph-berries graph))))

(defun conll-sentences-index (conll-sentences)
  (let* ((sentence-graphs
           (alexandria:alist-hash-table
            (mapcar (lambda (sent) (cons (cl-conllu:sentence->text sent) ; convert the obj to string
                                         (sentence->representation sent :debug-info nil)))
                    conll-sentences)
            :test #'equalp))
         (sentence-index (make-hash-table :test #'equalp)))
    (maphash
     (lambda (sentence graph)
       (dolist (index (graph-indices graph))
         (push sentence (gethash index sentence-index))))
     sentence-graphs)
    (list sentence-index sentence-graphs)))

(defun conll-file-index (conll-path)
  (conll-sentences-index (cl-conllu:read-conllu conll-path)))

(defun conll-sentences-query-index (conll-sentences query-string)
  (conll-sentences-index (remove-if (lambda (conll-sentence)
                                      (not (search query-string
                                                   (cl-conllu:sentence->text conll-sentence)
                                                   :test #'equalp)))
                                    conll-sentences)))

(defun index-summarize-cliques (index-list &key (minimum-size 3))
  (maphash (lambda (clique-subgraph clique-sentences)
             (when (<= minimum-size (length clique-sentences))
               (format t "~A ~A ~A~%"
                       clique-subgraph
                       ;; the shortest utterance.
                       (first (sort (copy-list clique-sentences) #'< :key #'length))
                       (length clique-sentences))))
           (car index-list)))

(defun index-strong-cliques (index-list &key (strength 2) (minimum-size 3))
  (let ((included-indices) ; the ones of required length
        (strong-clique-index (make-hash-table :test #'equalp)))
    (maphash (lambda (clique-subgraph clique-sentences)
               (when (<= minimum-size (length clique-sentences))
                 (push clique-subgraph included-indices)))
             (car index-list))
    (alexandria:map-combinations (lambda (clique-indices)
                                   (let ((clique-elements ; intersection of clique indices
                                           (reduce (lambda (sents-1 sents-2)
                                                     (intersection sents-1 sents-2 :test #'equalp))
                                                   (mapcar (lambda (index)
                                                             (gethash index (car index-list)))
                                                           clique-indices))))
                                     (when (<= 2 (length clique-elements))
                                       (setf (gethash (cl-strings:join clique-indices
                                                                       :separator ",")
                                                      strong-clique-index)
                                             clique-elements))))
                                 included-indices
                                 :length strength)
    strong-clique-index))

(defun conll-paragraph-index (conll-strings)
  "Index a list of paragraphs given as strings of ConLL-formatted sentences."
  (let ((paragraph-sentence-lists (mapcar (lambda (string)
                                            (let ((string-stream (make-string-input-stream string)))
                                              ;; read sentences from the string.
                                              (cl-conllu:read-stream string-stream)))
                                          conll-strings)))
    (mapcar (lambda (paragraph)
              ;; Make lists of sentence text + its representation indices.
              (mapcar (lambda (sentence-obj)
                        (cons (cl-conllu:sentence->text sentence-obj)
                              (graph-indices (sentence->representation sentence-obj))))
                      paragraph))
            paragraph-sentence-lists)))

(defun conll-file-paragraphs-index (conll-path)
  (conll-paragraph-index
   (do* ((conll-file (open conll-path :if-does-not-exist nil))
         (line (when conll-file (read-line conll-file))
               (read-line conll-file))
         (paragraph-strings))
        ((not line)
         (mapcar (lambda (lines) (cl-strings:join lines
                                                  :separator (coerce '(#\Newline) 'string)))
                 paragraph-strings))
     (if (equalp line "#####")
         (push nil paragraph-strings)
         (push line (first paragraph-strings))))))
