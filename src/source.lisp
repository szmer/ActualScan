(in-package :textviews)

(defclass source ()
  ((identifier :accessor source-identifier :initarg :identifier :type string)
   (format :accessor source-format :initarg :format :type string)
   (creator :accessor source-creator :initarg :creator :type string)
   (title :accessor source-title :initarg :title :type string)
   (publisher :accessor source-publisher :initarg :publisher :type string)
   (publication-place :accessor source-publication-place :initarg :publication-place :type string)
   (publication-date :accessor source-publication-date :initarg :publication-date :type string)
   (editor :accessor source-editor :initarg :editor :type string)
   (institution :accessor source-institution :initarg :institution :type string)
   (uri :accessor source-uri :initarg :uri :type string)
   (isbn :accessor source-isbn :initarg :isbn :type string)
   (doi :accessor source-doi :initarg :doi :type string)
   (meta-schemes :accessor source-meta-schemes :initarg :meta-schemes :type list :initform nil)
   (meta :accessor source-meta :initarg :meta :type hash-table :initform (make-hash-table :test #'equalp))
   (template-meta :accessor source-template-meta :initarg :template-meta :type hash-table :initform (make-hash-table :test #'equalp))))

(defun make-source (identifier format &rest other-keys)
  (apply #'make-instance
         (append (list 'source
                       :identifier identifier
                       :format format)
                 other-keys)))
