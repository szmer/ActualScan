(in-package :speechtractor)

(defclass paragraph ()
  ((path :accessor paragraph-path :initarg :path :initform nil)
   (text-nodes :accessor paragraph-text-nodes :initarg :text-nodes :initform nil)
   (classification :accessor paragraph-classification)
   (chars-count-in-links :accessor paragraph-chars-count-in-links :initform 0)
   (headingp :accessor paragraph-headingp :initarg :headingp :initform nil)
   (doc-startp :accessor paragraph-doc-startp :initarg :doc-startp :initform nil)))

(defun make-paragraph ()
  (make-instance 'paragraph))

(defun paragraph-text (paragraph &key (cleanp nil))
  (funcall
    (if cleanp #'cleaned-text #'identity)
    (with-output-to-string (result-str-stream)
      (let ((previous-node-text))
        (dolist (node-text (paragraph-text-nodes paragraph))
          (unless (or (null previous-node-text)
                      (unless (zerop (length previous-node-text))
                        (find (elt previous-node-text (1- (length previous-node-text)))
                              '(#\Space #\Newline #\Backspace #\Tab
                                #\Linefeed #\Page #\Return #\Rubout)))
                      (unless (zerop (length node-text))
                        (find (elt node-text 0)
                              '(#\Space #\Newline #\Backspace #\Tab
                                #\Linefeed #\Page #\Return #\Rubout))))
            (format result-str-stream " "))
          (setf previous-node-text node-text)
          (format result-str-stream "~A" node-text))))))

(defmethod print-object ((paragraph paragraph) stream)
  (print-unreadable-object (paragraph stream :type t :identity t)
    (format stream "(~A)~A"
           (ignore-errors (paragraph-classification paragraph))
           (cl-strings:shorten (paragraph-text paragraph :cleanp t) 60))))

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
