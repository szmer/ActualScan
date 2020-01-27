(in-package :speechtractor)

(defun value-judgment-p (classification)
  (find classification '(:good :near-good :bad)))

(defun classify-paragraphs-basic (paragraphs &key (classification-settings (make-hash-table)))
  (dolist (paragraph paragraphs paragraphs)
    (let* ((text (paragraph-text paragraph :cleanp t))
           (link-density (/ (paragraph-chars-count-in-links paragraph) 
                            (length text)))
           (stopword-density (paragraph-stopwords-density paragraph)))
      (setf (paragraph-classification paragraph)
            (cond
              ;; Bad if too many links.
              ((> link-density
                  (gethash :max-link-density classification-settings
                           *max-link-density-default*))
               :bad)
              ;; Bad if copyright sign.
              ((or (search "&copy" text) (find #\COPYRIGHT_SIGN text))
               :bad)
              ;; (Short paragraphs)
              ((< (length text)
                  (gethash :length-low classification-settings *length-low-default*))
               (if (not (zerop (paragraph-chars-count-in-links paragraph)))
                   ;; Bad if has links, or just short.
                   :bad :short))
              ;; Many stopwords.
              ((> stopword-density
                  (gethash :stopwords-high classification-settings
                           *stopwords-high-default*))
               (if (> (length text)
                      (gethash :length-high classification-settings *length-high-default*))
                   ;; High length - near-good. Moderate length - near-good.
                   :good :near-good))
              ;; Some stopwords - near-good.
              ((>= stopword-density
                   (gethash :stopwords-low classification-settings *stopwords-low-default*))
               :near-good)
              ;; Bad by default.
              (t :bad))))))

(defun revise-headings (initial-classification paragraphs
                         &key (classification-settings (make-hash-table)))
  "Compute distance from subsequent good paragraphs for paragraphs of initial-classification that \
   may be headings; if they are inside the max distance, they are good." 
  (let ((paragraph-n 0))
    (dolist (paragraph paragraphs paragraphs)
      (when (and (eq (paragraph-classification paragraph) initial-classification)
                 (paragraph-headingp paragraph)
                 (< (1+ paragraph-n) (length paragraphs)))
        (block near-headings
               (let ((distance 0))
                 (dolist (other-paragraph (subseq paragraphs (1+ paragraph-n)))
                   (if (eq (paragraph-classification other-paragraph) :good)
                       ;; If the subsequent paragraph is good, mark the heading as also good.
                       (progn 
                         (setf (paragraph-classification paragraph) :good)
                         (return-from near-headings))
                       ;; Otherwise count the distance.
                       (progn
                         (incf distance (length (paragraph-text other-paragraph :cleanp t))) 
                         (when (>= distance (gethash :max-heading-distance
                                                     *max-link-density-default*))
                           (return-from near-headings))))))))
      (incf paragraph-n))))

(defun classify-short-paragraphs (paragraphs &key (classification-settings (make-hash-table)))
  "If there the neighbors are only good and neargood, it is good, otherwise bad."
  (let ((paragraph-n 0))
    (dolist (paragraph paragraphs paragraphs)
      (when (eq (paragraph-classification paragraph) :short)
        (let ((prev-neighbor-classification (find-if #'value-judgment-p
                                                     (mapcar #'paragraph-classification paragraphs)
                                                     :end paragraph-n :from-end t))
              (next-neighbor-classification (find-if #'value-judgment-p
                                                     (mapcar #'paragraph-classification paragraphs)
                                                     :start (1+ paragraph-n))))
          (setf (paragraph-classification paragraph)
                (if (find
                      (list prev-neighbor-classification next-neighbor-classification)
                      '((:good :good) (:good :near-good) (:near-good :good) (:good nil) (nil :good))
                      :test #'equalp)
                    :good :bad))))
      (incf paragraph-n))))

(defun classify-near-good-paragraphs (paragraphs &key (classification-settings (make-hash-table)))
  "Only if surrounded by bad it is bad, otherwise good."
  (let ((paragraph-n 0))
    (dolist (paragraph paragraphs paragraphs)
      (when (eq (paragraph-classification paragraph) :near-good)
        (let ((prev-neighbor-classification (find-if #'value-judgment-p
                                                     (mapcar #'paragraph-classification paragraphs)
                                                     :end paragraph-n :from-end t))
              (next-neighbor-classification (find-if #'value-judgment-p
                                                     (mapcar #'paragraph-classification paragraphs)
                                                     :start (1+ paragraph-n))))
          (setf (paragraph-classification paragraph)
                (if (find (list prev-neighbor-classification next-neighbor-classification)
                          '((:bad :bad) (:bad nil) (nil :bad))
                          :test #'equalp)
                    :bad :good))))
      (incf paragraph-n))))

(defun classify-paragraphs (paragraphs &key (classification-settings (make-hash-table)))
  "Perform the paragraph classification."
  (classify-paragraphs-basic paragraphs :classification-settings classification-settings)
  (unless (gethash :no-headings classification-settings *no-headings-default*)
    (revise-headings :short paragraphs :classification-settings classification-settings))
  (classify-short-paragraphs paragraphs :classification-settings classification-settings)
  (classify-near-good-paragraphs paragraphs :classification-settings classification-settings)
  (unless (gethash :no-headings classification-settings *no-headings-default*)
    (revise-headings :bad paragraphs :classification-settings classification-settings))
  paragraphs)

(defun html-document-data (html-string metadata-funs
                                       &key (classification-settings (make-hash-table)))
  "Returns two values: list of paragraph objects and a list of document metadata property \
   lists for paragraphs marked as doc-startp. metadata-funs should be a property list \
   containing functions that take a plump node and a DOM path (as a list) and possibly return \
   relevant metadata. If no doc-startp function is provided, we use *doc-startp-default-fun*."
  (do* ((dom (plump:parse html-string))
        (paragraphs)
        (docs-metadata)
        (paths-nodes (mapcar (lambda (elem) (list nil elem))
                             (vector->list (plump:children dom))))
        (path-node (pop paths-nodes) (pop paths-nodes))
        (paragraph (make-paragraph)))
    ((null path-node)
     (progn (unless (paragraph-emptyp paragraph) (push paragraph paragraphs))
            (values
              (classify-paragraphs (reverse paragraphs)
                                   :classification-settings classification-settings)
              docs-metadata)))
    (destructuring-bind (path node) path-node
      (unless (and (plump:element-p node)
                   (find (plump:tag-name node) *skipped-tags* :test #'equalp))
      ;;;-(format t "~A: ~A~%" path node)
        (when (plump:element-p node) ; elements in plump have tags, but directly no text
          ;; Extend the path.
          (add-end path (plump:tag-name node))
          ;; Paragraph detection, starting documents.
          (labels ((%new-paragraph ()
                     (unless (paragraph-emptyp paragraph) ; othewise just reuse the previous object
                       (push paragraph paragraphs)
                       (setf paragraph (make-paragraph)))
                     (setf (paragraph-path paragraph) path)
                     (when (find-if (lambda (tag) (cl-ppcre:scan "^h\\d$" tag)) path)
                       (setf (paragraph-headingp paragraph) t))))
            ;; Start a new paragraph if need be.
            (when (or (find (plump:tag-name node) *paragraph-tags* :test #'equalp)
                    ;; These are cases where we left the paragraph tag:
                    (< (length path) (length (paragraph-path paragraph)))
                    (not (equalp (subseq path 0 (length (paragraph-path paragraph)))
                                 (paragraph-path paragraph))))
              (%new-paragraph))
            ;; Detect document starts.
            (when (and (not (paragraph-doc-startp paragraph)) ; silently ignore multiple signals
                    (funcall (getf metadata-funs :doc-startp *doc-startp-default-fun*) node path))
              (%new-paragraph) ; we may have to add one to avoid docstarting the previous one
              (setf (paragraph-doc-startp paragraph) t)
              (setf docs-metadata (append docs-metadata (list nil)))))
          ;; Links counting.
          (when (equalp "a" (plump:tag-name node))
            (incf (paragraph-chars-count-in-links paragraph)
                  (length (plump:render-text node))))
          ;; Metadata extraction.
          ;; Basically just add function values to metadata under whatever key they are under in the
          ;; metadata-funs.
          (when docs-metadata
            (let ((property-keys (remove-if (lambda (elem)
                                              (or (not (keywordp elem)) (eq :doc-startp elem)))
                                            metadata-funs)))
              (dolist (key property-keys)
                (let ((value (funcall (getf metadata-funs key) node path)))
                  (when value
                    (setf (getf (car (last docs-metadata)) key)
                          value)))))))
        ;; Add the text to the paragraph.
        (when (or (plump:textual-node-p node) (plump:text-node-p node))
          ;;;-(format t "~A~%" (cleaned-text (plump:text node)))
          (add-end (paragraph-text-nodes paragraph)
                   (cleaned-text (plump:text node)))
          ;;;-(format t "now ~A~%" (paragraph-text-nodes paragraph))
          )
        ;; Add the children at the front of the queque (depth-first).
        (when (plump:nesting-node-p node)
          (setf paths-nodes
                (append (mapcar (lambda (elem)
                                  (list (copy-list path) elem))
                                (vector->list (plump:children node)))
                        paths-nodes)))))))

;;;;; The general algorithm of paragraph classification is ported from Python justext package
;;;;; with the following license:
;;;;;
;;;;; Copyright (c) 2011, Jan Pomikalek <jan.pomikalek@gmail.com> Copyright (c) 2013, Michal Belica
;;;;; 
;;;;; All rights reserved.
;;;;; 
;;;;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;;;; 
;;;;; Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;;;; Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;;;; 
;;;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;; Ported by Szymon Rutkowski.
