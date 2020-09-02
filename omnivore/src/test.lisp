(defun run-old-tests ()
  ;; A test for graph structure.
  (let* ((test-graph
           (create-graph :syntax
                         (list "aaa" "bbb" "ccc" (list "ddd" :verbalp :graph-exit-p) "eee"
                               (list "fff" :verbalp))
                         ;; Two berries connected to ccc, ddd and fff, are verbals.
                         (list '(0 1) '(1 2 "pred") '(2 3) '(3 4) '(2 5))))
         (subgraph (subgraph-from (find-if (lambda (berry) (equalp (seme-label berry) "bbb"))
                                           (graph-berries test-graph))
                                  #'berry-verbalp)))
    (labels ((%good-stalk-p-fun (from-label to-label)
               (lambda (stalk)
                 (and (equalp (seme-label (stalk-from stalk)) from-label)
                      (equalp (seme-label (stalk-to stalk)) to-label)))))
      (format t "The subgraph representation: ~A~%"
              subgraph)
      (format t "The subgraph should contain two berries. ~A~%"
              (= (length (graph-berries subgraph)) 2))
      (format t "The subgraph should contain bbb and ccc. ~A ~A~%"
              (truep (find-if (lambda (berry) (equalp (seme-label berry) "bbb"))
                              (graph-berries subgraph)))
              (truep (find-if (lambda (berry) (equalp (seme-label berry) "ccc"))
                              (graph-berries subgraph))))
      (format t "bbb (the root) should be first. ~A~%"
              (eq 0 (position "bbb" (graph-berries subgraph)
                              :test #'equalp :key #'seme-label)))
      (format t "The subgraph should contain only one stalk, between bbb and ccc ~A ~A~%"
              (= (length (graph-stalks subgraph)) 1)
              (truep (find-if (%good-stalk-p-fun "bbb" "ccc")
                              (graph-stalks subgraph))))
      (format t "The bbb should have only one stalk, between bbb and ccc ~A ~A~%"
              (ignore-errors
               (= (length (berry-stalks
                           (find-if (lambda (berry) (equalp (seme-label berry) "bbb"))
                                    (graph-berries subgraph))))
                  1))
              (ignore-errors
               (truep (find-if (%good-stalk-p-fun "bbb" "ccc")
                               (berry-stalks
                                (find-if (lambda (berry) (equalp (seme-label berry) "bbb"))
                                         (graph-berries subgraph)))))))
      (format t "The ccc should have only one stalk, between bbb and ccc ~A ~A~%"
              (ignore-errors
               (= (length (berry-stalks
                           (find-if (lambda (berry) (equalp (seme-label berry) "ccc"))
                                    (graph-berries subgraph))))
                  1))
              (ignore-errors
               (truep (find-if (%good-stalk-p-fun "bbb" "ccc")
                               (berry-stalks
                                (find-if (lambda (berry) (equalp (seme-label berry) "ccc"))
                                         (graph-berries subgraph)))))))
      (format t "The bbb should lead to the same ccc that is included in the graph berry list ~A~%"
              (ignore-errors
               (eq (find-if (lambda (berry) (equalp (seme-label berry) "ccc"))
                            (graph-berries subgraph))
                   (stalk-to (first (berry-stalks
                                     (find-if (lambda (berry) (equalp (seme-label berry) "bbb"))
                                              (graph-berries subgraph))))))))))
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
  ;; Notes for testing.
  ;;(mapcar #'cl-conllu:token-id (sentence-semantic-token-order (fifth *sents*)))
  ;;(4 8 3 22 9 7 5 2 1 23 21 20 19 11 6 24 12 10 25 16 28 17 15 27 18 14 13 26)
  (list "anyone" "audio" "audiophile" "bass" "bright" "can" "comfortable" "detail" "difficult"
        "ear" "easy" "experience" "friend" "forum" "gear"
        "hard" "harsh" "head" "headphone" "high" "hobby" "idea" "imaging"
        "music" "musical" "mid" "midrange" "natural" "neutral" "order"
        "pad" "person" "price" "product" "recording" "someone" "sound" "soundstage" "small"
        "treble" "warm" "volume")

  ;; A test for in-xml.
  (let* ((xml-string
           ;; One challenge here is that the first node is assigned id 4.
           "<?xml version='1.0' encoding='utf-8'?>   <gexf xmlns='http://www.gexf.net/1.3' version='1.3' xmlns:viz='http://www.gexf.net/1.3/viz' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.gexf.net/1.3 http://www.gexf.net/1.3/gexf.xsd'>   <meta>   <creator>Therminsley Omnivore early version</creator>   <description></description>   </meta>   <graph defaultedgetype='directed' mode='static'>   <nodes>   <node id='4' label='_sth_kicks_sth'>    <viz:size value='10'/>                <viz:color r='255' g='0' b='0'/>   </node>   <node id='1' label='something'>    <viz:size value='10'/>                <viz:color r='255' g='0' b='0'/>   </node>   <node id='2' label='sticky'>    <viz:size value='10'/>                <viz:color r='255' g='0' b='0'/>   </node>   </nodes>   <edges>   <edge id='0' label='obj' source='4' target='1'/>            <edge label='' id='1' source='1' target='2' weight='0.8'/>   </edges>   </graph>   </gexf>"
           )
         (graph-spec (graph-spec-from-xml xml-string))
         (graph-specced-ok
           (equalp
            graph-spec
            (list
             ;; The nodes should be in the correct order, and the root first.
             '(("_sth_kicks_sth" :verbalp :obj-exit-p) ("something") ("sticky"))
             ;; We will receive edges reversed in order.
             '((1 2 :explication-prototype) (0 1 "obj"))))))
    (format t "Graph properly specced from XML ~A~%" graph-specced-ok)
    (when (not graph-specced-ok) (print graph-spec) (format t "~%")))
  )
