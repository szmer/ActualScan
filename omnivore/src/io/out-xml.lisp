;;;;
;;;; Printing semantic graphs to XML (Gephi .gph files).
;;;;
(in-package :omnivore)

(defgeneric xml-basic-representation (obj id)
  (:documentation
   "Return an unevaluated s-expression that, passed to XML-EMITTER, will print the object to XML. The
  id supplied may be used for referring to the object in the representation. The 'basic'
  representation, derived directly from the object, most has to be augmented with some logic aware
  of the whole graph."))

;;; In the high-level tags, we make the attribute lists with list and not quote to facilitate
;;; appending.

(defmethod xml-basic-representation ((obj berry) id)
  "Users probably want to append a properly computed viz:position tag with x and y attributes to the
quoted list."
  `(xml-emitter:with-tag ("node" (list '("id" ,id) '("label" ,(seme-label obj))))
     (xml-emitter:empty-tag "viz:size" '(("value" 10)))
     (xml-emitter:empty-tag "viz:color" '(("r" 255) ("g" 0) ("b" 0)))))

(defmethod xml-basic-representation ((obj stalk) id)
  "Users need to add to the attributes (last element of the quoted list) source and target
attributed with existing node (berry) ids."
  `(xml-emitter:empty-tag "edge" (list '("id" ,id) '("label" ,(seme-label obj)))))

(defun graph->xml (graph &optional (output-stream *standard-output*))
  (let ((berry->id (alexandria:alist-hash-table
                    (mapcar (lambda (berry berry-id) (cons berry berry-id)) ; don't pack the assoc
                                                                            ; cdr in a list
                            (graph-berries graph)
                            (alexandria:iota (length (graph-berries graph)))))))
    (xml-emitter:with-xml-output (output-stream :encoding "utf-8")
      (xml-emitter:with-tag
          ("gexf" (list
                   '("xmlns" "http://www.gexf.net/1.3")
                   '("version" "1.3")
                   '("xmlns:viz" "http://www.gexf.net/1.3/viz")
                   '("xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance")
                   '("xsi:schemaLocation"
                     "http://www.gexf.net/1.3 http://www.gexf.net/1.3/gexf.xsd")))
        (xml-emitter:with-tag ("meta")
          (xml-emitter:simple-tag "creator" "Therminsley Omnivore early version")
          (xml-emitter:simple-tag "description" ""))
        (xml-emitter:with-tag ("graph" '(("defaultedgetype" "directed") ("mode" "static")))
          ;; Berries -> nodes.
          (xml-emitter:with-tag ("nodes")
            (mapcar (lambda (berry) (eval (xml-basic-representation berry (gethash berry berry->id))))
                    (graph-berries graph)))
          ;; Stalks -> edges.
          (xml-emitter:with-tag ("edges")
            (mapcar (lambda (stalk stalk-id)
                      (let ((representation-list (xml-basic-representation stalk stalk-id)))
                        (appendf (car (last representation-list)) ; the last element contains the
                                 ; attribute list
                                 ;; We need to add a quoted 2-list to a quoted s-expression.
                                 (list `'("source" ,(gethash (stalk-from stalk) berry->id))
                                       `'("target" ,(gethash (stalk-to stalk) berry->id))
                                       `'("weight" ,(gethash (seme-creator stalk)
                                                             *creator->stalk-weight*))))
                        (eval representation-list)))
                    (graph-stalks graph)
                    (alexandria:iota (length (graph-stalks graph))))))))))
;;--- (defparameter *g* (create-graph :syntax (list "aaa" "bbb" (list "ccc" :verbalp :graph-exit-p)) (list '(0 1) '(2 0 "pred"))))
;;---
;;---
;;---<?xml version="1.0" encoding="utf-8"?>
;;---<gexf xmlns="http://www.gexf.net/1.3" version="1.3" xmlns:viz="http://www.gexf.net/1.3/viz" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.gexf.net/1.3 http://www.gexf.net/1.3/gexf.xsd">
;;---    <meta>
;;---        <creator>Therminsley Omnivore early version</creator>
;;---        <description></description>
;;---    </meta>
;;---    <graph defaultedgetype="directed" mode="static">
;;---        <nodes>
;;---            <node id="0" label="aaa">
;;---                <viz:size value="10"/>                <viz:color r="255" g="0" b="0"/>
;;---            </node>
;;---            <node id="1" label="bbb">
;;---                <viz:size value="10"/>                <viz:color r="255" g="0" b="0"/>
;;---            </node>
;;---            <node id="2" label="ccc">
;;---                <viz:size value="10"/>                <viz:color r="255" g="0" b="0"/>
;;---            </node>
;;---        </nodes>
;;---        <edges>
;;---            <edge id="0" label="" source="0" target="1"/>            <edge id="1" label="pred" source="2" target="0"/>
;;---        </edges>
;;---    </graph>
;;---</gexf>
