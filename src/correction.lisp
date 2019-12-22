(in-package :textviews)

(defclass correction ()
  ((attr-name :accessor correction-attr-name :initarg :attr-name :type string)
   (replaced-value :accessor correction-replaced-value :initarg :replaced-value :type string)
   (authority :accessor correction-authority :initarg :authority :type string)))

(defun make-correction (attr-name replaced-value authority)
  (make-instance 'correction :attr-name attr-name :replaced-value replaced-value :authority authority))
