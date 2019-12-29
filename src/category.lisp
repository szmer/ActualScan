(in-package :textviews)

(deftype category-criterion ()
  '(member equal like less more less-equal more-equal))

;;;; TODO maybe map criteria to functions?

(defclass category ()
  ((division-kind :accessor category-division-kind :initarg :division-kind :type record-kind)
   (divisions-range :accessor category-divisions-range :initarg :divisions-range :type cons :initform nil)
   (attribute-name :accessor category-attribute-name :initarg :attribute-name :type string)
   (attribute-value :accessor category-attribute-value :initarg :attribute-value)
   (criterion :accessor category-criterion :initarg :criterion :type category-criterion)))

(defun make-category (division-kind &key divisions-range attribute-name attribute-value criterion)
  (declare (type string attribute-name) (type category-criterion criterion))
  (make-instance 'category :division-kind division-kind :divisions-range divisions-range
                 :attribute-name attribute-name :attribute-value attribute-value
                 :criterion criterion))
