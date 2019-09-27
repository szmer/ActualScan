;;;;
;;;; Converting graphs from xML to our internal representation.
;;;;
(declaim (optimize (debug 3)))
(in-package :omnivore)

(defun graph-spec-from-xml (xml-string)
  (let ((xml-structure (plump:parse xml-string)))
    (list
      ;; Node (berry) specs.
      (mapcar (lambda (node)
                (let ((label-spec (list (plump:attribute node "label"))))
                  (if (equalp "_sth_" (subseq (first label-spec) 0 5))
                      (append label-spec
                              (if (equalp "_sth_is_" (first label-spec))
                                  '(:verbalp)
                                  '(:verbalp :obj-exit-p)))
                      ;; The normal, non-verbal case.
                      label-spec)))
              ;; NOTE We currently get those from plump reversed, and the order matters (esp. the
              ;; root being first).
              (reverse
               (plump:get-elements-by-tag-name xml-structure "node")))
      ;; Edge (stalk) specs.
      (mapcar (lambda (edge)
                (append
                  (list (read-from-string (plump:attribute edge "source"))
                        (read-from-string (plump:attribute edge "target")))
                  ;; Add a label if there is one defined.
                  (list-wrapped
                    (and (plump:attribute edge "label") 
                         (< 0 (length 
                                (plump:attribute edge "label")))
                         ;; (repeat to return it)
                         (plump:attribute edge "label")))
                  (list-wrapped
                    (case (and (plump:attribute edge "weight")
                               (read-from-string (plump:attribute edge "weight")))
                      (1.0 :explication-definition)
                      (0.8 :explication-prototype)))))
              (plump:get-elements-by-tag-name xml-structure "edge")))))

;;; A test.
(let* ((xml-string
         "<?xml version='1.0' encoding='utf-8'?>   <gexf xmlns='http://www.gexf.net/1.3' version='1.3' xmlns:viz='http://www.gexf.net/1.3/viz' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://www.gexf.net/1.3 http://www.gexf.net/1.3/gexf.xsd'>   <meta>   <creator>Therminsley Omnivore early version</creator>   <description></description>   </meta>   <graph defaultedgetype='directed' mode='static'>   <nodes>   <node id='0' label='_sth_kicks_sth'>    <viz:size value='10'/>                <viz:color r='255' g='0' b='0'/>   </node>   <node id='1' label='something'>    <viz:size value='10'/>                <viz:color r='255' g='0' b='0'/>   </node>   <node id='2' label='sticky'>    <viz:size value='10'/>                <viz:color r='255' g='0' b='0'/>   </node>   </nodes>   <edges>   <edge id='0' label='obj' source='0' target='1'/>            <edge label='' id='1' source='1' target='2' weight='0.8'/>   </edges>   </graph>   </gexf>"
         )
       (graph-spec (graph-spec-from-xml xml-string))
       (graph-specced-ok
         (equalp
           graph-spec 
           (list '(("_sth_kicks_sth" :verbalp :obj-exit-p) ("something") ("sticky"))
                 ;; We will receive edges reversed in order.
                 '((1 2 :explication-prototype) (0 1 "obj"))))))
  (format t "Graph properly specced from XML ~A~%" graph-specced-ok)
  (when (not graph-specced-ok) (print graph-spec) (format t "~%")))
