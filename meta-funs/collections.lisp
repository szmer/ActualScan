(in-package :speechtractor)

;;;; It makes more sense to just try everything we got on a website based on a rough
;;;; classification. Versions of software, such as phpbb, are hard to detect without trying
;;;; everything anyway. Manually mapping each site to functions would probably be too much hassle
;;;; and we should try to manage this with modifications and tests.

(defparameter *source-type-meta-funs*
  (alexandria:alist-hash-table
    (list (list "forums"
                :docstart-p #'forums-docstart
                :date-post #'forums-date
                :author #'forums-author
                :url #'forums-permalink)
          (list "test"
                :author (lambda (node path)
                          (when (plump:has-attribute node "author")
                            (plump:attribute node "author"))))
          ;; Empty defaults.
          (cons nil nil))
    :test #'equalp))

(defparameter *source-type-classification-settings*
  (alexandria:alist-hash-table
    (list (cons "forums"
                ;; relaxed parameters for forums.
                (alexandria:plist-hash-table
                 '(:stopwords-low 0.17 :stopwords-high 0.22 :length-high 110
                  ;; but at least be more stringent about link density
                  :max-link-density 0.16)))
          ;; Empty defaults.
          (cons "test" (make-hash-table))
          (cons nil (make-hash-table)))
    :test #'equalp))
