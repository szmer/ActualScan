;;;;
;;;; *Semes* are referred to as the basic units of meaning in traditional linguistics. Here, we use
;;;; *the word as an abstraction for berries and stalks (nodes and edges) of semantic representation
;;;; *graphs.
;;;;
(in-package :omnivore)

;;;
;;; Auxillary types.
;;;
(deftype element-creator () '(member
                             :explication-definition
                             :explication-prototype
                             :unknown-token
                             :syntax))
(defparameter *creator->stalk-weight*
  (alexandria:alist-hash-table
   (list '(:explication-definition 1.0)
         '(:explication-prototype 0.8)
         '(:unknown-token 0.55)
         '(:syntax 0.5))))

(deftype direction () '(member :from :to))
(defun opposite (direction)
  (declare (type direction direction))
  (ecase direction (:from :to) (:to :from)))

;;;
;;; Main types.
;;;
(defclass seme ()
  ((label :reader seme-label :initarg :label :initform "-" :type string)
   (creator :reader seme-creator :initarg :creator :type element-creator)))

(defclass berry (seme)
  ((verbalp :reader berry-verbalp :initarg :verbalp :initform nil :type boolean)
   ;; important if this is a verbal, whether it takes an obj exit:
   (obj-exit-p :reader berry-obj-exit-p :initarg :obj-exit-p :initform nil :type boolean)
   (stalks :accessor berry-stalks :initform nil :type list)
   ;;
   ;; Valence correction slots.
   (valence-instructions :reader berry-valence-instructions :initarg :valence-instructions
                         :initform nil :type list)
   ;; this is set to t after valence correcting is ran, which should execute all instructions
   ;; from the appropriate slot
   (valence-corrected :accessor berry-valence-corrected :initform nil :type boolean)))
(defmethod print-object ((obj berry) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a/~a" (seme-label obj) (seme-creator obj))))

(defun copy-berry (original-berry &key (keep-old-stalks nil))
  (let ((new-berry
          (make-instance 'berry
                         :label (seme-label original-berry) :creator (seme-creator original-berry)
                         :verbalp (berry-verbalp original-berry)
                         :obj-exit-p (berry-obj-exit-p original-berry)
                         :valence-instructions (copy-list
                                                (berry-valence-instructions original-berry)))))
    (setf (slot-value new-berry 'valence-corrected)
          (berry-valence-corrected original-berry))
    ;; Replace the stalk with copied ones with the new berry substituted for the original one.
    (if keep-old-stalks
        (setf (berry-stalks new-berry)
              (copy-list (berry-stalks original-berry)))
        (setf (berry-stalks new-berry)
              (mapcar (lambda (stalk)
                        (let ((new-stalk (copy-stalk stalk)))
                          (if (eq (stalk-from new-stalk) original-berry)
                              (setf (stalk-from new-stalk) new-berry)
                              (setf (stalk-to new-stalk) new-berry))
                          new-stalk))
                      (berry-stalks original-berry))))
    new-berry))

(defclass stalk (seme)
    ((weight :reader stalk-weight :initarg :weight :type real)
     (from :accessor stalk-from :initarg :from :type berry)
     (to :accessor stalk-to :initarg :to :type berry)))
(defmethod print-object ((obj stalk) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A-~A->~A"
            (ignore-errors (seme-label (stalk-from obj))) ; show NIL if invalid
            (if (slot-boundp obj 'label)
                (concatenate 'string "[" (seme-label obj) "]")
                "")
            (ignore-errors (seme-label (stalk-to obj))))))

(defun copy-stalk (original-stalk)
  (make-instance 'stalk
                 :label (seme-label original-stalk) :creator (seme-creator original-stalk)
                 :from (stalk-from original-stalk) :to (stalk-to original-stalk)
                 :weight (stalk-weight original-stalk)))

(defun stalk-not-registered-p (prospective-stalk)
  "Check that the berry led to has no existing stalk from the origin berry, and vice versa."
  ;; We have to check both stalk lists to prevent pushing the stalk if it appears on one
  ;; of them.
  (and (not (find-if (lambda (stalk) (eq (stalk-from prospective-stalk)
                                         (stalk-from stalk)))
                     (berry-stalks (stalk-to prospective-stalk))))
       (not (find-if (lambda (stalk) (eq (stalk-to prospective-stalk)
                                         (stalk-to stalk)))
                     (berry-stalks (stalk-from prospective-stalk))))))

(defun register-stalk (prospective-stalk)
  "Ensure that the stalk is added to the adjacency lists of its berries. We raise a recoverable
error if a stalk between the berries already exists. Returns the stalk."
  (if (stalk-not-registered-p prospective-stalk)
      (progn
        (push prospective-stalk (berry-stalks (stalk-from prospective-stalk)))
        (push prospective-stalk (berry-stalks (stalk-to prospective-stalk)))
        prospective-stalk)
      (cerror "a stalk already exists between berries" 'simple-error)))

(defun connect-with-stalk (berry1 berry2 creator &key (label ""))
  "Connects both nodes with a fresh stalk and adjoins it to their adjacency lists. The stalk is
  returned. We raise a recoverable error if a stalk between the berries already exists."
  (let ((stalk (make-instance 'stalk :from berry1 :to berry2 :label label
                                     :creator creator
                                     :weight (gethash creator *creator->stalk-weight*))))
    (register-stalk stalk)))

(defun stalk-connect (direction stalk target-berry)
  (case direction
    (:from (setf (stalk-from stalk) target-berry) stalk)
    (:to (setf (stalk-to stalk) target-berry) stalk))
  (register-stalk stalk))
