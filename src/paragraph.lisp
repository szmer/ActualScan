(in-package :speechtractor)

(defclass paragraph ()
  ((path :accessor paragraph-path :initarg :path :initform nil)
   (text-nodes :accessor paragraph-text-nodes :initarg :text-nodes :initform nil)
   (classification :accessor paragraph-classification)
   (chars-count-in-links :accessor paragraph-chars-count-in-links :initform 0)
   (headingp :accessor paragraph-headingp :initarg :headingp :initform nil)
   (doc-startp :accessor paragraph-doc-startp :initarg :doc-startp :initform nil)
   ;; a property list:
   (doc-metadata :accessor paragraph-doc-metadata :initarg :doc-metadata :initform nil)))

(defun make-paragraph ()
  (make-instance 'paragraph))

(defun paragraph-text (paragraph &key (cleanp nil))
  (funcall (if cleanp #'cleaned-text #'identity)
           (apply #'concatenate (append '(string)
                                        (paragraph-text-nodes paragraph)))))

(defun paragraph-emptyp (paragraph)
  (zerop (length (paragraph-text paragraph :cleanp t))))

(defun paragraph-stopwords-density (paragraph)
  (let* ((text (string-downcase
                 ;; remove all punctuation
                 (cl-ppcre:regex-replace-all "[\\-\\:;\\.,\\?!\\(\\)\\[\\]'’‘\\\"]"
                                          (paragraph-text paragraph :cleanp t)
                                          "")))
         (tokens (cl-strings:split text))
         (count 0))
    (dolist (token tokens)
      (when (gethash token *stopwords*)
        (incf count)))
    (/ count (length tokens))))
