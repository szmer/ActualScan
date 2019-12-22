(in-package :textviews)

(defclass source ()
  ((unique-name :accessor source-unique-name :initarg :unique-name :type string)
   (format :accessor source-format :initarg :format :type string)
   (uri :accessor source-uri :initarg :uri :type string)
   (isbn :accessor source-isbn :initarg :isbn :type string)
   (doi :accessor source-doi :initarg :doi :type string)
   (author :accessor source-author :initarg :author :type string)
   (title :accessor source-title :initarg :title :type string)
   (editor :accessor source-editor :initarg :editor :type string)
   (institution :accessor source-institution :initarg :institution :type string)
   (place :accessor source-place :initarg :place :type string)
   (time :accessor source-time :initarg :time :type local-time:timestamp)
   (meta :accessor source-meta :initarg :meta :type hash-table)))

(defun make-source (unique-name format &rest other-keys)
  (apply #'make-instance
         (append (list 'source
                       :unique-name unique-name
                       :format format)
                 other-keys)))
