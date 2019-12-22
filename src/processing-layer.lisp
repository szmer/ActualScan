(in-package :textviews)

(defclass processing-layer ()
  ((mechanism :accessor processing-mechanism :initarg :mechanism :type string)
   (is-sourcing :accessor processing-is-sourcing :initarg :is-sourcing :type boolean)
   (time-started :accessor processing-time-started :initarg :time-started :type local-time:timestamp)
   (time-ended :accessor processing-time-ended :initarg :time-ended :type local-time:timestamp)
   (source :accessor processing-source :initarg :source :type source)
   (parameters :accessor processing-parameters :initarg :parameters :type hash-table)))

(defun make-processing-layer (mechanism time-started time-ended &key source parameters)
  (let ((layer (make-instance 'processing-layer
                 :mechanism mechanism
                 :time-started time-started
                 :time-ended time-ended))))
  (if source
      (setf (processing-source layer) source
            (processing-is-sourcing layer) t)
      (setf (processing-is-sourcing layer) nil))
  (when parameters (setf processing-parameters parameters)))
