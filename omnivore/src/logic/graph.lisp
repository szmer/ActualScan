;;;;
;;;; Semantic graphs form the actual representations of meanings.
;;;;
;;;; They are originally formed from dependency syntax parse trees. Graphs can have their units
;;;; (berries) unfolded, eventually to be constructed only from semantic primes and unknown token
;;;; marks.
;;;;
(in-package :omnivore)

(defclass graph ()
  ((berries :accessor graph-berries :initarg :berries :initform nil
            :type (proper-list berry))
   (stalks :accessor graph-stalks :initarg :stalks :initform nil
           :type (proper-list stalks))))
;; TODO handle the case where there are many
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
                                                  :label (or (third stalk-spec) "")))
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
        ;; Paths = individual stalks leading us "forward". We don't need to store trails, since we
        ;; move in one direction in a graph guaranteed to accomodate for that.
        (paths
         ;; remove stalks that would move us backwards.
         (remove-if (lambda (stalk) (eq (funcall stalk-forward-function stalk)
                                        start-berry))
                    (berry-stalks start-berry)))
        (current-path (pop paths) (pop paths)))
       ((not current-path) (values nil nil)) ; the not found case
    ;; If we are sure that a current-path exists, we can extract the next berry it leads to.
    (let* ((next-berry (funcall stalk-forward-function current-path)))
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
