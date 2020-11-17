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
        ;; Assemble the text from all the text nodes covered by the paragraph.
        (dolist (node-text (paragraph-text-nodes paragraph))
          ;; Add an additional space unless it's the first node, or there is already whitespace at
          ;; the boundary, or the node being added is empty.
          (unless (or (null previous-node-text)
                      (zerop (length (cleaned-text node-text)))
                      (unless (zerop (length previous-node-text))
                        (find (elt previous-node-text (1- (length previous-node-text)))
                              '(#\Space #\Newline #\Backspace #\Tab
                                #\Linefeed #\Page #\Return #\Rubout)))
                      (unless (zerop (length node-text))
                        (find (elt node-text 0)
                              '(#\Space #\Newline #\Backspace #\Tab
                                #\Linefeed #\Page #\Return #\Rubout))))
            (princ " " result-str-stream))
          (setf previous-node-text node-text)
          ;; Use princ instead of format to avoid padding other "niceties" meant for line printing
          (princ node-text result-str-stream))))))

(defmethod print-object ((paragraph paragraph) stream)
  (print-unreadable-object (paragraph stream :type t :identity t)
    (format stream "(~A)~A"
           (ignore-errors (paragraph-classification paragraph))
           (cl-strings:shorten (paragraph-text paragraph :cleanp t) 60))))

(defun paragraph-emptyp (paragraph)
  (zerop (length (paragraph-text paragraph :cleanp t))))

(defun text-stopwords-density (text)
  (let* ((text (string-downcase
                 ;; remove all punctuation
                 (cl-ppcre:regex-replace-all "[\\-\\:;\\.,\\?!\\(\\)\\[\\]'’‘\\\"]"
                                          text "")))
         (tokens (cl-strings:split text))
         (count 0))
    (dolist (token tokens)
      (when (gethash token *stopwords*)
        (incf count)))
    (/ count (length tokens))))

(defun paragraph-stopwords-density (paragraph)
  (text-stopwords-density (paragraph-text paragraph :cleanp t)))
