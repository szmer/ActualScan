;;;;; This defines two abstract classes, text-record and text-object, that let views and corpora
;;;;; share some slots and methods with divisions.

(in-package :textviews)

(deftype record-kind ()
  '(member :corpus :document :section :sentence :token))

(defclass text-record ()
  ((identifier :accessor record-identifier :initarg :identifier :type string)
   (kind :accessor record-kind :initarg :record-kind :type record-kind)
   (parent :accessor record-parent :initarg :parent :type text-record)
   (language :accessor record-language :initarg :language :type string)
   (creator :accessor record-creator :initarg :creator :type string)
   (publication-date :accessor record-publication-time :initarg :publication-time :type string)
   (meta-schemes :accessor record-meta-schemes :initarg :meta-schemes :type hash-table
                 :initform (make-hash-table :test #'equalp))
   (meta :accessor record-meta :initarg :meta :type hash-table
         :initform (make-hash-table :test #'equalp))
   (corrections :accessor record-corrections :initarg corrections :type vector :initform #1A())
   (deferrables :accessor record-deferrables :initarg :deferrables :type hash-table
                :initform (list-as-hash-set '("publication-date" "language" "source"
                                              "creator" "source-region" "meta-schemes")))))

(defun read-attribute (text-record attribute-name)
  "Read attribute of the text-record, or if it is deferrable possibly retrieve it from higher the descendancy chain."
  (declare (type text-record text-record) (type string attribute-name))
  (let ((attribute-symbol (alexandria:make-keyword (string-upcase attribute-name))))
    (cond ((and (slot-exists-p text-record attribute-symbol)
                (slot-boundp text-record attribute-symbol))
           (slot-value text-record attribute-symbol))
          ((gethash attribute-name (record-meta attribute-name))
           (gethash attribute-name (record-meta attribute-name)))
          ((and (slot-boundp text-record 'parent)
                (find attribute-name (record-deferrables text-record) :test #'equalp))
           (read-attribute (record-parent text-record) attribute-name)))))
