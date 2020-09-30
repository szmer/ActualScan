;;;;
;;;; Converting graphs from xML to our internal representation.
;;;;
(declaim (optimize (debug 3)))
(in-package :omnivore)

(defun graph-spec-from-xml (xml-string)
  "The xml-string is valid gexf file contents. Note that you may want to change the node order
   after construing, if the root is not under the 0 index. A proper input to create-graph function
   is returned."
  (let* ((xml-structure (plump:parse xml-string))
         ;; Map node indices that are used in edges (specified in XML) to their actual, positional
         ;; indices.
         (gexf->positional-node-index (make-hash-table))
         (dom-nodes (plump:child-elements (xml-path xml-structure "gexf" "graph" "nodes")))
         (dom-edges (plump:get-elements-by-tag-name xml-structure "edge")))
    (list
      ;; Node (berry) specs.
      (map 'list
           (lambda (node node-n)
             ;; Save the positional node n.
             (setf (gethash (read-from-string (plump:attribute node "id"))
                            gexf->positional-node-index)
                   node-n)
             ;; Produce the label, and possibly other berry specifications.
             (let ((label-spec (list (plump:attribute node "label"))))
               (if (and (< 5 (length (first label-spec)))
                        (equalp "_sth_" (subseq (first label-spec) 0 5)))
                   (append label-spec
                           (if (equalp "_sth_is_" (first label-spec))
                               '(:verbalp)
                               '(:verbalp :obj-exit-p)))
                   ;; The normal, non-verbal case.
                   label-spec)))
           dom-nodes
           (alexandria:iota (length dom-nodes)))
      ;; Edge (stalk) specs.
      (mapcar (lambda (edge)
                (append
                  ;; Translate the node indices given in gexf to positional ones.
                  (list (gethash (read-from-string (plump:attribute edge "source"))
                                 gexf->positional-node-index)
                        (gethash (read-from-string (plump:attribute edge "target"))
                                 gexf->positional-node-index))
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
              dom-edges))))
