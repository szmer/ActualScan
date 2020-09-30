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
                :skip-p (lambda (node path)
                          (or (blog-skip-p node path)
                              (general-skip-p node path)))
                ;; having a docstart at the beginning of the document lets us capture on meta in the
                ;; <head> because the document is already started
                :doc-startp #'full-html-doc-startp
                :date_post #'media-date
                :author #'blog-author
                :meta-burner #'general-meta-burner)
          (list "media"
                :skip-p #'general-skip-p
                :doc-startp #'full-html-doc-startp
                :date_post #'media-date
                :author #'media-author
                :meta-burner #'general-meta-burner)
          ;; When interpreting search pages, we interpret each found link as a document with only
          ;; the permalink.
          (list "searchpage"
                ;:skip-p #'general-skip-p
                ;; find document items and next page links on the search page (note that often they
                ;; have very similar style)
                :doc-startp #'searchpage-doc-startp
                ;; is the link to another search page?
                :is_search #'searchpage-searchp
                ;; url element of the specific document item
                :url #'searchpage-permalink)
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
                  ;; media can have some pretty short paragraphs for effect and link a lot
                  '(:length-high 120 :max-link-density 0.4)))
          (cons "searchpage"
                ;; relaxed parameters for forums.
                (alexandria:plist-hash-table
                 '(:stopwords-low 0.17 :stopwords-high 0.22 :length-low 40 :length-high 40)))
          ;; Empty defaults.
          (cons "test" (make-hash-table))
          (cons nil (make-hash-table)))
    :test #'equalp))

(defun interpret-file-as* (source-type file-path &key (initial-only nil))
  "A debug function to see how a html file is interpreted."
  ;; For example:
  ;; (interpret-file-as* "forums" (asdf:system-relative-pathname 'speechtractor "test/pages/styleforum-xenforo.html"))
  (html-document-data (uiop:read-file-string file-path)
                      (gethash source-type *source-type-meta-funs*)
                      :initial-only initial-only
                      :classification-settings
                      (gethash source-type *source-type-classification-settings*)))
