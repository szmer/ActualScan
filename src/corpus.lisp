(in-package :textviews)

(defclass corpus (text-record)
  ((version :accessor corpus-version :initarg :version :type string)
   (contact :accessor corpus-contact :initarg :contact :type string)
   (maintainer :accessor corpus-maintainer :initarg :maintainer :type string)
   (sources :accessor :corpus-sources :initarg :sources :type array :initform #1A())
   (processing-layers :accessor :corpus-processing-layers :initarg :processing-layers :type array :initform #1A())
   (omited-attributes :accessor :corpus-omited-attributes :initarg :omited-attribute :type list :initform nil)))

(defun make-corpus (identifier version &rest other-args)
  "A corpus making function, that accepts also keyword arguments applicable for text records."
  (apply #'make-instance
         (append (list 'corpus :kind 'corpus :identifier identifier :version version)
                 other-args)))

;;;; TODO
(defun read-corrected-attribute (corpus processing-layer-id division attribute-name)
  nil)
