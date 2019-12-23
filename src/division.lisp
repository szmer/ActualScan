(in-package :textviews)

(defclass division (text-object text-record)
  ((source :accessor record-source :initarg :source :type source)
   (source-region :accessor record-source-region :initarg :source-region :type string)))
   

(defclass document (division)
  ((title :accessor document-title :initarg title :type string)))

(defclass section (division)
  ((degree :accessor section-degree :initarg degree :type integer)
   (title :accessor section-title :initarg title :type string)))

(defclass sentence (division)
  ())

(defclass token (division)
  ((lemma :accessor token-lemma :initarg lemma :type string)
   (pos :accessor token-pos :initarg pos :type string)
   (form-description :accessor token-form-description :initarg form-description :type string)))

(defun make-division (kind parent identifier contents &rest other-args)
  "Make an instance of division of the given kind, possibly with a non-nil parent, with contents supplied as a list (sub-divisions of the division) or a string (its raw text). The other-args are used to fill other slots of the object."
  (declare (type record-kind kind) (type text-record parent) (type string identifier))
  (when (eq kind 'corpus) (error "Make-division function should not be used for corpora"))
  (apply #'make-instance
         (append 
           (list kind
                 :identifier identifier)
           (when parent (list :parent parent))
           (if (typep contents 'list)
               (list :divisions contents)
               (list :raw-text contents))
           other-args)))
