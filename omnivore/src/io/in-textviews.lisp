(in-package :omnivore)

;;; NOTE deduplication here noticeably increases the time, up to seconds!!

(defun tokens-sents (tv-tokens &key (deduplicate t))
  (if deduplicate
      (remove-duplicates
        (mapcar (lambda (token) (record-parent token))
                tv-tokens)
        :test #'equalp :key #'raw-text)
      (mapcar (lambda (token) (record-parent token))
              tv-tokens)))

(defun tokens-section-sents (tv-tokens &key (deduplicate t))
  (do* ((sections)
       (tokens-iterated (copy-list tv-tokens))
       (token (pop tokens-iterated) (pop tokens-iterated)))
      ((null token)
       (if deduplicate
           (remove-duplicates
             (reduce #'append (mapcar #'division-divisions sections))
             :test #'equalp :key #'raw-text)
           (reduce #'append (mapcar #'division-divisions sections))))
      ;; The second parent (of the string is the section.
      (pushnew (record-parent (record-parent token)) sections)))
