(in-package :textviews)

(defclass view (text-object)
  ((corpus :accessor view-corpus :initarg :corpus :type corpus)
   (method :accessor view-method :initarg :method :type string)
   (category :accessor view-category :initarg :category :type category)
   (subviews :accessor view-subviews :initarg :subviews :type array :initform #1A())
   (values :accessor category-value :initarg :values :type hash-table :initform (make-hash-table :test #'equalp))))

(defun make-view (corpus &key method category subviews descendants)
  (make-instance 'view :corpus corpus :method method :subviews subviews :descendants descendants))
