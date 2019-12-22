(in-package :textviews)

(deftype category-criterion ()
  '(member equal like less more less-equal more-equal))

;;;; TODO maybe map criteria to functions?

(defclass category ()
  ((division-kind :accessor category-division-kind :initarg :division-kind :type division-kind)
   (divisions-range :accessor category-divisions-range :initarg :divisions-rage :type cons :initform nil)
   (attribute-name :accessor category-attribute-name :initarg :attribute-name :type string)
   (attribute-value :accessor category-attribute-value :initarg :attribute-value :type string)
   (criterion :accessor category-criterion :initarg :criterion :type category-criterion)))

