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
