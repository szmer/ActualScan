;;;;
;;;; This file implements logic for using the berry-stalk-graph representation for our semantic
;;;; metalanguage.
;;;;
(declaim (optimize (debug 3)))
(in-package :omnivore)

(defparameter *prefix-unknown-token* "??;")
(defparameter *prefix-proper-name-token* "==;")

;;;
;;; Main semantic "exits" of the structures.
;;;
;;; NOTE all these follow a simple-minded approach, that can be then corrected with valence
;;; instructions.
;;;

(defmacro define-graph-stalk-function (exit-name &key stalk-label)
  "Provided that the graph-(exit-name) function exists, define a function
graph-(exit-name)-dangling-stalk returning a dangling stalk to/from the exit in question with (
creator direction graph) arguments, which, in case the important berry is a verbal, is properly
labeled - with the exit-name by default."
  (let ((exit-function-symbol
          (read-from-string (concatenate 'string "graph-" exit-name "-berry"))))
    `(defun
         ;; assemble the function name
         ,(read-from-string (concatenate 'string "graph-" exit-name "-dangling-stalk"))
         ;; the arguments:
         (creator direction graph)
       "Return a dangling stalk from/to the graph's exit. It should be completed with the
connect-stalk function."
       (let ((important-berry (,exit-function-symbol graph)))
         (if (berry-verbalp important-berry)
             ;; (note that we use the direction as source of the appropriate keyword argument)
             (make-instance 'stalk :creator creator (the keyword direction) important-berry
                                   :label ,(or stalk-label exit-name)
                                   :weight (gethash creator *creator->stalk-weight*))
             (make-instance 'stalk :creator creator (the keyword direction) important-berry
                                   :weight (gethash creator *creator->stalk-weight*)))))))

(defun graph-root-berry (graph) (first (graph-berries graph)))
;; TODO make it always run when the macro is redefined!
(define-graph-stalk-function "root" :stalk-label "pred")

(defun graph-subj-berry (graph)
  (if (or (berry-verbalp (graph-root-berry graph))
          (equalp "something" (seme-label (graph-root-berry graph))))
      (graph-root-berry graph)
      (error "don't know how to extract subj from the graph")))
(define-graph-stalk-function "subj")

(defun graph-obj-berry (graph)
  (if (berry-verbalp (graph-root-berry graph))
      (graph-root-berry graph)
      ;; Technically it can be a *something* plugged into some verbal, but the verbal must be
      ;; equivalent to it per our graph representation semantics.
      ;; TODO alternatively search for this something with a good test predicate?
      (or (nearest-from (graph-root-berry graph) #'berry-verbalp)
          (error "don't know how to extract obj from the graph"))))
(define-graph-stalk-function "obj")

(defun graph-pred-berry (graph)
  (if (berry-verbalp (graph-root-berry graph))
      (graph-root-berry graph)
      (or (nearest-from (graph-root-berry graph) #'berry-verbalp)
          (error "don't know how to extract pred from the graph"))))
(define-graph-stalk-function "pred")

(defun graph-sit-berry (graph)
  (if (berry-verbalp (graph-root-berry graph))
      (graph-root-berry graph)
      (or (nearest-from (graph-root-berry graph) #'berry-verbalp)
          (error "don't know how to extract sit from the graph"))))
(define-graph-stalk-function "sit")

;;; NOTE we currently ignore the direction here when constructing the graph, to preserve always
;;; stalking from the root outward. (All backwards relations connect to root to avoid exits
;;; unavailable in graph children)
;;;
;;; Incidentally, almost all :forward relations are stricly to root.
(defun connection-graph (semantic-direction stalk-fun from-graph to-graph
                         &key debug-dependency-label)
  "Returns a graph necessary to connect the two graphs. The semantic-direction is either :forward or
:backward, looking from the root."
  (declare (type keyword semantic-direction) (type graph from-graph to-graph))
  (let* ((main-stalk-direction (ecase semantic-direction
                                 (:forward :to) (:backward :from)))
         (main-stalk-from-graph (ecase semantic-direction
                                  (:forward from-graph) (:backward to-graph)))
         (main-stalk-to-graph (ecase semantic-direction
                                (:forward to-graph) (:backward from-graph)))
         ;; This may be the only one, or the one leading from the proxy berry. NOTE it may be
         ;; principally *from* the from-graph, if the semantic-direction is :backward.
         (main-stalk (funcall stalk-fun
                              :syntax main-stalk-direction main-stalk-to-graph)))
    (when debug-dependency-label
      (setf (slot-value main-stalk 'label)
            (concatenate 'string (seme-label main-stalk) ":::" debug-dependency-label)))
    ;; If the stalk is between two verbal berries, we need to ensure the proper semantic direction
    ;; with a proper intermediate `something` berry. Otherwise, we ignore the issue ???? (for now).
    ;;;---(when (or (equalp (seme-label (graph-root-berry to-graph)) "??;the")
    ;;;---          (equalp (seme-label (graph-root-berry from-graph)) "??;the"))
    ;;;---  (break))
    (if (and (berry-verbalp (graph-root-berry from-graph))
             (berry-verbalp (graph-root-berry to-graph)))
        (let ((proxy-berry (make-instance 'berry :label "something" :creator :syntax))
              (proxy-stalk (graph-root-dangling-stalk :syntax
                                                      (opposite main-stalk-direction)
                                                      main-stalk-from-graph)))
          (make-instance 'graph
                         :berries (list proxy-berry)
                         ;; Connect the proxy berry to the main graphs.
                         :stalks (list (stalk-connect main-stalk-direction proxy-stalk proxy-berry)
                                       (stalk-connect (opposite main-stalk-direction) main-stalk
                                                      proxy-berry))))
        ;; The simple case, no verbals.
        (make-instance 'graph
                       :berries ()
                       :stalks (list
                                (stalk-connect
                                 (opposite main-stalk-direction)
                                 main-stalk
                                 (graph-root-berry main-stalk-from-graph)))))))
