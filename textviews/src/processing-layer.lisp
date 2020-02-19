(in-package :textviews)

(defclass processing-layer ()
  ((identifier :accessor processing-identifier :initarg :identifier :type string)
   (mechanism :accessor processing-mechanism :initarg :mechanism :type string)
   (parameters :accessor processing-parameters :initarg :parameters :type hash-table)))

(defun make-processing-layer (identifier mechanism &key parameters)
  (let ((layer (make-instance 'processing-layer :mechanism mechanism :identifier identifier)))
    (when parameters
      (setf (processing-parameters layer) parameters))
    layer))
