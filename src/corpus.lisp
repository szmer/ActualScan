(in-package :textviews)

(defclass corpus (text-record)
  ((version :accessor corpus-version :initarg :version :type string)
   (contact :accessor corpus-contact :initarg :contact :type string)
   (maintainer :accessor corpus-maintainer :initarg :maintainer :type string)
   (sources :accessor corpus-sources :initarg :sources :type hash-table
            :initform (make-hash-table :test #'equalp))
   (processing-layers :accessor corpus-processing-layers :initarg :processing-layers :type hash-table
                      :initform (make-hash-table :test #'equalp))
   (omited-attributes :accessor corpus-omited-attributes :initarg :omited-attributes :type hash-table
                      :initform (make-hash-table :test #'equalp))))

(defun make-corpus (identifier version sources processing-layers &rest other-args)
  "A corpus making function, that accepts also keyword arguments applicable for text records."
  (apply #'make-instance
         (append (list 'corpus :record-kind 'corpus :identifier identifier :version version
                       :sources sources :processing-layers processing-layers)
                 other-args)))

;;;; TODO
(defun read-corrected-attribute (corpus processing-layer-id division attribute-name)
  nil)
