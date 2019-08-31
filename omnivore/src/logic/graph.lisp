;;;;
;;;; Semantic graphs form the actual representations of meanings.
;;;;
;;;; They are originally formed from dependency syntax parse trees. Graphs can have their units
;;;; (berries) unfolded, eventually to be constructed only from semantic primes and unknown token
;;;; marks.
;;;;
(declaim (optimize (debug 3)))
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
                (append (subseq (graph-berries obj) 0 10) '(and more))
                (graph-berries obj))
            (if (list-longer-p (graph-stalks obj) 10)
                (append (subseq (graph-stalks obj) 0 10) '(and more))
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
including) berries are encountered that satisfy the test-function. The returned subgraph consists
entirely of newly copied berries and stalks."
  (when backwards-to-root-p
    (cerror "Currently there is a problem with going backwards where root may not originate all
            connections. Maybe there should be code for reversing all stalks in that case."
            nil))
  (labels ((%prune-berry (berry pruned-function)
             "Remove all the stalks pointing in the direction given by pruned-function."
             (setf (berry-stalks berry)
                   (remove-if (lambda (stalk)
                                ;; remove if pruned-function points to something else.
                                (not (eq (funcall pruned-function stalk) berry)))
                              (berry-stalks berry))))
           (%copy-pruned (berry pruned-function)
             "Return a copy of the berry where all the stalks pointing in the direction given by
             pruned-function are removed."
             (let ((copied-berry (copy-berry berry)))
               (%prune-berry copied-berry pruned-function)
               copied-berry))
           (%tracing-stalks-from (new-berry old-berry stalk-forward-function)
             "Return new stalks leading from new-berry, being copies of that originating from
             old-berry."
             (remove nil
                     (mapcar (lambda (stalk)
                               ;; remove stalks that would move us backwards.
                               (unless (eq (funcall stalk-forward-function stalk)
                                           old-berry)
                                 ;; point the path stalks backwards to the new berry.
                                 (let ((tracing-stalk (copy-stalk stalk)))
                                   (if backwards-to-root-p
                                       (setf (stalk-to tracing-stalk) new-berry)
                                       (setf (stalk-from tracing-stalk) new-berry))
                                   tracing-stalk)))
                             (berry-stalks old-berry))))
           (%fix-backstalk (backstalk new-berry direction)
             "Replace the target berry of the backstalk (the back being in direction) with new-berry
             and replace the old stalk on the both ends of backstalk. Return the backstalk. The
             backstalk should probably be a tracing stalk obtained from another local function."
             (let ((other-berry
                     (if (eq direction :from)
                         (stalk-from backstalk) (stalk-to backstalk)))
                   (former-berry ; equivalent to new-berry in the original graph
                     (if (eq direction :from)
                         (stalk-to backstalk) (stalk-from backstalk))))
               ;; Update the backstalk.
               (if (eq direction :from)
                   (setf (stalk-to backstalk) new-berry)
                   (setf (stalk-from backstalk) new-berry))
               ;; Remove the old stalk equivalent to backstalk.
               (setf (berry-stalks new-berry)
                     (remove-if (lambda (berry-stalk) (or (eq (stalk-to berry-stalk) former-berry)
                                                          (eq (stalk-from berry-stalk)
                                                              former-berry)))
                                (berry-stalks new-berry)))
               (setf (berry-stalks other-berry)
                     (remove-if (lambda (berry-stalk) (or (eq (stalk-to berry-stalk) former-berry)
                                                          (eq (stalk-from berry-stalk)
                                                              former-berry)))
                                (berry-stalks other-berry)))
               (push backstalk (berry-stalks new-berry))
               (push backstalk (berry-stalks other-berry)))
             backstalk))
    (do* ((stalk-forward-function (if backwards-to-root-p #'stalk-from #'stalk-to))
          ;; (note that the flag is not default!)
          (stalk-backward-function (if backwards-to-root-p #'stalk-to #'stalk-from))
          ;; Make a copy of the start berry (without stalks that would move us backwards from the
          ;; berry).
          (center-berry (%copy-pruned start-berry stalk-backward-function))
          ;; We can access the previous berry to have its stalk cut (if we end on it) from the stalk.
          (paths (%tracing-stalks-from center-berry start-berry stalk-forward-function))
          (current-path (pop paths) (pop paths))
          ;;
          ;; Construing the returned subgraph.
          (subgraph-berries (list center-berry))
          (subgraph-stalks))
         ((not current-path)
          (make-instance 'graph :berries (reverse subgraph-berries) ; root has to be first
                                :stalks subgraph-stalks))
      ;; If we are sure that a current-path exists, we can extract the next berry it leads to.
      (let ((next-berry (funcall stalk-forward-function current-path)))
        (if (funcall test-function next-berry)
            ;; Cut off if the test function succeeded.
            ;; (The end-berry is the one leading us here, while the would-be next-berry is left out)
            (let* ((end-berry (funcall stalk-backward-function current-path)))
              ;; Remove everything that would lead us farther. (we don't need to update the
              ;; subgraph-stalks, since they're only backwards and already accounted for)
              (%prune-berry end-berry stalk-forward-function))
            ;; Continue if the test function failed.
              (let ((copied-berry (copy-berry next-berry :keep-old-stalks t)))
                ;; The current-path is already a tracing stalk that needs to have its target changed
                ;; to our copy and be registered on its ends. The other direction should be safe to
                ;; replace their stalks, as it should be only our new copies this way.
                (push (%fix-backstalk current-path copied-berry (if backwards-to-root-p :to :from))
                      subgraph-stalks)
                (push copied-berry subgraph-berries)
                (setf paths
                      ;; Append new paths leading from the next-berry at the end, to ensure a
                      ;; breadth-first search.
                      (append paths
                              (%tracing-stalks-from copied-berry next-berry stalk-forward-function)))))))))

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
