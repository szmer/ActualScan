(in-package :speechtractor)

;;;; It makes more sense to just try everything we got on a website based on a rough
;;;; classification. Versions of software, such as phpbb, are hard to detect without trying
;;;; everything anyway. Manually mapping each site to functions would probably be too much hassle
;;;; and we should try to manage this with modifications and tests.

(defparameter *source-type-meta-funs*
  (alexandria:alist-hash-table
    (list (list "forums"
                :skip-p #'general-skip-p
                :doc-startp #'forums-doc-startp
                ;; cl-json will preserve the underscore in date_post, and re-read as date--post
                :date_post #'forums-date
                :author #'forums-author
                :url #'forums-permalink)
          (list "blog"
                :skip-p #'general-skip-p
                :meta-burner #'general-meta-burner) 
          (list "media"
                :skip-p #'general-skip-p
                :doc-startp #'full-html-doc-startp
                :date_post #'media-date
                :meta-burner #'general-meta-burner) 
          ;; Empty defaults.
          (list "test"
                :author (lambda (node path)
                          (when (plump:has-attribute node "author")
                            (plump:attribute node "author"))))
          (cons nil nil))
    :test #'equalp))

(defparameter *source-type-classification-settings*
  (alexandria:alist-hash-table
    (list (cons "forums"
                ;; relaxed parameters for forums.
                (alexandria:plist-hash-table
                 '(:stopwords-low 0.17 :stopwords-high 0.22 :length-low 40 :length-high 40)))
          (cons "blog" (make-hash-table))
          (cons "media"
                (alexandria:plist-hash-table
                  ;; media can have some pretty short paragraphs for effect
                  '(:length-low 60)))
          ;; Empty defaults.
          (cons "test" (make-hash-table))
          (cons nil (make-hash-table)))
    :test #'equalp))

(defun interpret-file-as* (source-type file-path &key (class "message"))
  "A debug function to see how a html file is interpreted."
  ;; For example:
  ;; (interpret-file-as* "forums" (asdf:system-relative-pathname 'speechtractor "test/pages/styleforum-xenforo.html"))
  (html-document-data (uiop:read-file-string file-path)
                      (gethash source-type *source-type-meta-funs*)
                      :classification-settings
                      (gethash source-type *source-type-classification-settings*)))
