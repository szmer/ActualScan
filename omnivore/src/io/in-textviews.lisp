(in-package :omnivore)

(defun tokens-sents (tv-tokens)
  (mapcar (lambda (token) (record-parent token))
          tv-tokens))
