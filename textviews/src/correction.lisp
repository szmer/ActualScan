(in-package :textviews)

(defclass correction ()
  ((attribute-name :accessor correction-attribute-name :initarg :attribute-name :type string)
   (replaced-value :accessor correction-replaced-value :initarg :replaced-value :type string)
   (processing-layer :accessor correction-processing-layer :initarg :processing-layer :type processing-layer)
   (time :accessor correction-time :initarg :time :type local-time:timestamp)))

(defun make-correction (attribute-name replaced-value processing-layer &key (time local-time:now))
  (make-instance 'correction :attribute-name attribute-name :replaced-value replaced-value
                 :processing-layer processing-layer :time time))
