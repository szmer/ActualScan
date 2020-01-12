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
      ;; The second parent of the token is the section.
      (pushnew (record-parent (record-parent token)) sections)))

(defun tokens-sents-with-windows (window-side-size tv-tokens &key (deduplicate t))
  (do* ((returned-sents)
        (tokens-iterated (copy-list tv-tokens))
        (token (pop tokens-iterated) (pop tokens-iterated)))
    ((null token)
     (if deduplicate
         (remove-duplicates returned-sents :test #'equalp :key #'raw-text)
         returned-sents))
    (let* ((token-sentence (record-parent token))
           (sentence-n (position token-sentence
                                 (division-divisions (record-parent token-sentence)))))
    (setf returned-sents
          (append returned-sents
                  ;; The sentence's parent is the section.
                  (subseq (division-divisions (record-parent token-sentence))
                          (max 0 (- sentence-n window-side-size))
                          (min (+ sentence-n window-side-size)
                               (length (division-divisions (record-parent token-sentence))))))))))

(defun sentence-tree (tv-sentence)
  (when (gethash "conll_tree" (record-meta tv-sentence))
    (with-input-from-string (stream (gethash "conll_tree" (record-meta tv-sentence)))
      ;; (we get a list of sentences)
      (first (cl-conllu:read-conllu stream)))))
