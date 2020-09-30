(in-package :omnivore)

(defun tokens-sections (tv-tokens &key (deduplicate t))
  (if deduplicate
      ;; NOTE We can deduplicate by object identity (#'eq) as long as string duplicates are filtered
      ;; when loading from Solr.
      (remove-duplicates
        (mapcar (lambda (token) (record-parent (record-parent token)))
                tv-tokens))
      (mapcar (lambda (token) (record-parent (record-parent token)))
              tv-tokens)))

(defun tokens-sents (tv-tokens &key (deduplicate t))
  (if deduplicate
      ;; NOTE We can deduplicate by object identity (#'eq) as long as string duplicates are filtered
      ;; when loading from Solr.
      (remove-duplicates
        (mapcar (lambda (token) (record-parent token))
                tv-tokens))
      (mapcar (lambda (token) (record-parent token))
              tv-tokens)))

(defun tokens-section-sents (tv-tokens &key (deduplicate t))
  "Get all sentences of the sections where the tokens belong."
  (do* ((sections)
        (tokens-iterated (copy-list tv-tokens))
        (token (pop tokens-iterated) (pop tokens-iterated)))
      ((null token)
       (if deduplicate
           (remove-duplicates
             (reduce #'append (mapcar #'division-divisions sections)))
           (reduce #'append (mapcar #'division-divisions sections))))
      ;; The second parent of the token is the section.
      (pushnew (record-parent (record-parent token)) sections)))

(defun tokens-sents-with-windows (window-side-size tv-tokens &key (deduplicate t))
  "Get tokens sentences with windows of neighboring sentences of the desired size."
  (do* ((returned-sents)
        (tokens-iterated (copy-list tv-tokens))
        (token (pop tokens-iterated) (pop tokens-iterated)))
    ((null token)
     (if deduplicate
         (remove-duplicates returned-sents)
         returned-sents))
    (let* ((token-sentence (record-parent token))
           (sentence-n (position token-sentence
                                 (division-divisions (record-parent token-sentence)))))
      ;; The sentence's parent is the section.
      (dolist (sent (subseq (division-divisions (record-parent token-sentence))
                            (max 0 (- sentence-n window-side-size))
                            (min (+ sentence-n window-side-size)
                                 (length (division-divisions (record-parent token-sentence))))))
        (push sent returned-sents)))))

(defun sentence-tree (tv-sentence)
  (when (gethash "conll_tree" (record-meta tv-sentence))
    (with-input-from-string (stream (gethash "conll_tree" (record-meta tv-sentence)))
      ;; (we get a list of sentences)
      (first (cl-conllu:read-conllu stream)))))
