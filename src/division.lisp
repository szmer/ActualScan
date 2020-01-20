(in-package :textviews)

(defclass division (text-record)
  ((raw-text :accessor division-raw-text :initarg :raw-text :type string)
   ;; use ways of in-place insertion in processing: https://stackoverflow.com/questions/4387570/in-common-lisp-how-can-i-insert-an-element-into-a-list-in-place
   (divisions :accessor division-divisions :initarg :divisions :type list)
   (source :accessor division-source :initarg :source :type source)
   (source-region :accessor division-source-region :initarg :source-region :type string)))
   

(defclass document (division)
  ((title :accessor document-title :initarg :title :type string)))

(defclass section (division)
  ((degree :accessor section-degree :initarg :degree :type integer)
   (title :accessor section-title :initarg :title :type string)))

(defclass sentence (division)
  ())

(defclass token (division)
  ((lemma :accessor token-lemma :initarg :lemma :type string)
   (pos :accessor token-pos :initarg :pos :type string)
   (form-description :accessor token-form-description :initarg :form-description :type string)))

(defun make-division (kind parent identifier contents &rest other-args)
  "Make an instance of division of the given kind, possibly with a non-nil parent, with contents supplied as a list (sub-divisions of the division) or a string (its raw text). The other-args are used to fill other slots of the object."
  (declare (type record-kind kind) (type string identifier)
           ;(type text-record parent)
           )
  (when (eq kind :corpus) (error "Make-division function should not be used for corpora"))
  (apply #'make-instance
         (append 
           (list (find-symbol (symbol-name kind) 'textviews)
                 :record-kind kind
                 :identifier identifier)
           (when parent (list :parent parent))
           (if (typep contents 'list)
               (list :divisions contents)
               (list :raw-text contents))
           other-args)))


;;;TODO handle multiple divisions with the same identifier but different versions or retrieval time
(defun raw-text (division)
  "Retrieve the raw text either from the slot or division's divisions."
  (declare (type division division))
  (if (slot-boundp division 'raw-text)
      (division-raw-text division)
      (let ((text
              (with-output-to-string (str)
                (dolist (descendant (division-divisions division))
                  (format str " ~A" (raw-text descendant))))))
        (if (< 0 (length text))
            (subseq text 1)
            text))))
