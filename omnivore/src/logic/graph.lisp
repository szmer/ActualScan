;;;;
;;;; Semantic graphs form the actual representations of meanings.
;;;;
;;;; They are originally formed from dependency syntax parse trees. Graphs can have their units
;;;; (berries) unfolded, eventually to be constructed only from semantic primes and unknown token
;;;; marks.
;;;;
(in-package :omnivore)

(defclass graph ()
  ;; The first berry should be the graph root, from which all connections originate.
  ((berries :accessor graph-berries :initarg :berries :initform nil
            :type (proper-list berry))
   (stalks :accessor graph-stalks :initarg :stalks :initform nil
           :type (proper-list stalks))))
(defmethod print-object ((obj graph) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~%berries:~A~%stalks:~A~%"
            (if (list-longer-p (graph-berries obj) 10)
                (subseq (graph-berries obj) 0 10)
                (graph-berries obj))
            (if (list-longer-p (graph-stalks obj) 10)
                (subseq (graph-stalks obj) 0 10)
                (graph-stalks obj)))))

(defun create-graph (creator berry-specs stalk-specs)
  "Intended for new graphs. The creator applies for all created semes, and also for determining edge
weights. Berries are defined by labels (optionally in a list with :verbalp and :obj-exit-p) and
stalks by berry indices."
  (let ((berries (mapcar
                  (lambda (berry-spec)
                    (let ((berry-spec (if (listp berry-spec) berry-spec
                                          (list berry-spec))))
                      (make-instance 'berry
                                     :label (first berry-spec)
                                     :creator creator
                                     :verbalp (truep (find :verbalp berry-spec))
                                     :obj-exit-p (truep (find :obj-exit-p berry-spec)))))
                  berry-specs)))
    (make-instance 'graph
                   :berries berries
                   :stalks (mapcar
                            (lambda (stalk-spec)
                              (connect-with-stalk (nth (first stalk-spec) berries)
                                                  (nth (second stalk-spec) berries)
                                                  creator
                                                  :label (or (third stalk-spec) "-")))
                            stalk-specs))))

(defun concatenate-graphs (&rest graphs)
  "Return a graph object concatenating graphs' berry and stalk lists. No actual connection check is
performed. The first graph's root becomes the root of the result."
  (make-instance 'graph
                 :berries (reduce #'append (mapcar #'graph-berries graphs))
                 :stalks (reduce #'append (mapcar #'graph-stalks graphs))))

(defun nearest-from (start-berry test-function &key (backwards-to-root-p nil))
  "Return the nearest berry that satisties the test-function. Unless backwards-to-root-p, we move
 only with stalks away from the root (or towards it otherwise). Return nil if no such berry can be
 find. The exact berry returned is undefined if there are multiple nearest ones that satisfy the
 test-function."
  (do* ((stalk-forward-function (if backwards-to-root-p #'stalk-to #'stalk-from))
        ;; Paths = *individual stalks* leading us "forward". We don't need to store trails, since we
        ;; move in one direction in a graph guaranteed to accomodate for that.
        ;; (note that remove-if creates a new list, so we don't ruin the original with popping)
        (paths
         ;; remove stalks that would move us backwards from the start-berry.
         (remove-if (lambda (stalk) (eq (funcall stalk-forward-function stalk)
                                        start-berry))
                    (berry-stalks start-berry)))
        (current-path (pop paths) (pop paths)))
       ((not current-path) nil) ; the not found case
    ;; If we are sure that a current-path exists, we can extract the next berry it leads to.
    (let ((next-berry (funcall stalk-forward-function current-path)))
      (if (funcall test-function next-berry)
          ;; The success return:
          (return-from nearest-from next-berry)
          (setf paths
                ;; Append new paths leading from the next-berry at the end, to ensure a
                ;; breadth-first search.
                (append paths
                        ;; remove stalks that would move us backwards.
                        (remove-if (lambda (stalk) (eq (funcall stalk-forward-function
                                                                stalk)
                                                       next-berry))
                                   (berry-stalks next-berry))))))))

(defun subgraph-from (start-berry test-function &key (backwards-to-root-p nil))
  "Return the subgraph containing berries and stalks originating from the start-berry, until (not
including) berries are encountered that satisfy the test-function. All the berries and stalks in the
result graph are references to the original ones, except for the first berry and the boundary ones,
which are replaced with copies with out-of-subgraph stalks removed."
  (when backwards-to-root-p
    (cerror "Currently there is a problem with going backwards where root may not originate all
            connections. Maybe there should be code for reversing all stalks in that case."
            nil))
  (labels ((%copy-pruned (berry pruned-function)
             "Return a copy of the berry where all the stalks pointing in the direction given by
             pruned-function are removed."
             (let ((copied-berry (copy-berry berry)))
               (setf (berry-stalks copied-berry)
                     (remove-if (lambda (stalk)
                                  ; remove if pruned-function points to something else.
                                  (not (eq (funcall pruned-function stalk) copied-berry)))
                                (berry-stalks copied-berry)))
               copied-berry)))
    (do* ((stalk-forward-function (if backwards-to-root-p #'stalk-from #'stalk-to))
          ;; (note that the flag is not default!)
          (stalk-backward-function (if backwards-to-root-p #'stalk-to #'stalk-from))
          ;; Make a copy of the start berry (without stalks that would move us backwards from the
          ;; berry).
          (center-berry (%copy-pruned start-berry stalk-backward-function))
          ;; We can access the previous berry to have its stalk cut (if we end on it) from the stalk.
          (paths (copy-list (berry-stalks center-berry)))
          (current-path (pop paths) (pop paths))
          ;;
          ;; Construing the returned subgraph.
          (subgraph-berries (list center-berry))
          (subgraph-stalks))
         ((not current-path)
          (make-instance 'graph :berries subgraph-berries :stalks subgraph-stalks))
      ;; If we are sure that a current-path exists, we can extract the next berry it leads to.
      (let ((next-berry (funcall stalk-forward-function current-path)))
        (if (funcall test-function next-berry)
            ;; Cut off if the test function succeeded.
            (setf subgraph-berries
                  (n-replace-once subgraph-berries
                                  (funcall stalk-backward-function current-path)
                                  ;; Remove everything that would lead us farther.
                                  (%copy-pruned (funcall stalk-backward-function current-path)
                                                stalk-forward-function)))
            ;; Continue if the test function failed.
            (progn
              (push next-berry subgraph-berries)
              (push current-path subgraph-stalks)
              (setf paths
                    ;; Append new paths leading from the next-berry at the end, to ensure a
                    ;; breadth-first search.
                    (append paths
                            ;; remove stalks that would move us backwards.
                            (remove-if (lambda (stalk) (eq (funcall stalk-forward-function
                                                                    stalk)
                                                           next-berry))
                                       (berry-stalks next-berry))))))))))

(let* ((test-graph
         (create-graph :syntax
                       (list "aaa" "bbb" "ccc" (list "ddd" :verbalp :graph-exit-p) "eee")
                       (list '(0 1) '(1 2 "pred") '(2 3) '(3 4))))
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
                                       (graph-berries subgraph)))))))))
