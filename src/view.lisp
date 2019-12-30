(in-package :textviews)

(defclass view ()
  ((corpus :accessor view-corpus :initarg :corpus :type corpus)
   (corpus-time :accessor view-corpus-time :initarg :corpus-time :type local-time:timestamp)
   (method :accessor view-method :initarg :method :type string)
   (categories :accessor view-categories :initarg :categories :type list :initform nil)
   (divisions :accessor view-divisions :initarg :divisions :type list :initform nil)
   (subviews :accessor view-subviews :initarg :subviews :type list :initform nil)
   (values :accessor category-value :initarg :values :type hash-table :initform (make-hash-table :test #'equalp))
   (omited-attributes :accessor view-omited-attributes :initarg :omited-attributes :type hash-table
                      :initform (make-hash-table :test #'equalp))))

(defun make-view (corpus &key corpus-time categories subviews divisions omited-attributes)
  (apply #'make-instance
         (append (list 'view :corpus corpus)
                 (when corpus-time (list :corpus-time corpus-time))
                 (when categories (list :categories categories))
                 (when subviews (list :subviews subviews))
                 (when divisions (list :divisions divisions))
                 (when omited-attributes (list :omited-attributes omited-attributes)))))

;;; Querying all subviews could be used as "OR".
