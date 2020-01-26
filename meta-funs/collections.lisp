(in-package :speechtractor)

;;;; It makes more sense to just try everything we got on a website based on a rough
;;;; classification. Versions of software, such as phpbb, are hard to detect without trying
;;;; everything anyway. Manually mapping each site to functions would probably be too much hassle
;;;; and we should try to manage this with modifications and tests.

(defparameter *forums-funs*
  '(:docstart-p #'forums-docstart
    :date-post #'forums-date
    :author #'forums-author
    :url #'forums-permalink))
