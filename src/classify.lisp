(in-package :speechtractor)

(defun has-value-judgment-p (paragraph)
  (or (eq (paragraph-classification paragraph) :good)
      (eq (paragraph-classification paragraph) :near-good)
      (eq (paragraph-classification paragraph) :bad)))

(defun classify-paragraphs-basic (paragraphs &key (classification-settings (make-hash-table)))
  (dolist (paragraph paragraphs)
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

(defun classify-short-paragraphs (paragraphs)
  "If there the neighbors are only good and neargood, it is good, otherwise bad."
  (let ((paragraph-n 0))
    (dolist (paragraph paragraphs paragraphs)
      (when (eq (paragraph-classification paragraph) :short)
        (let ((prev-neighbor (find-if #'has-value-judgment-p paragraphs :end paragraph-n :from-end t
                                      :key #'paragraph-classification))
              (next-neighbor (find-if #'has-value-judgment-p paragraphs :start paragraph-n
                                      :key #'paragraph-classification)))
          (setf (paragraph-classification paragraph)
                (if (find
                      (list prev-neighbor next-neighbor)
                      '((:good :good) (:good :near-good) (:near-good :good) (:good nil) (nil :good))
                      :test #'equalp)
                    :good :bad))))
      (incf paragraph-n))))

(defun classify-near-good-paragraphs (paragraphs)
  "Only if surrounded by bad it is bad, otherwise good."
  (let ((paragraph-n 0))
    (dolist (paragraph paragraphs paragraphs)
      (when (eq (paragraph-classification paragraph) :short)
        (let ((prev-neighbor (find-if #'has-value-judgment-p paragraphs :end paragraph-n :from-end t
                                      :key #'paragraph-classification))
              (next-neighbor (find-if #'has-value-judgment-p paragraphs :start paragraph-n
                                      :key #'paragraph-classification)))
          (setf (paragraph-classification paragraph)
                (if (find (list prev-neighbor next-neighbor) '((:bad :bad) (:bad nil) (nil :bad))
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

(defun html-document-data (html-string &key (classification-settings (make-hash-table)))
  (do* ((dom (plump:parse html-string))
        (paragraphs)
        (paths-nodes (mapcar (lambda (elem) (list nil elem))
                             (vector->list (plump:child-elements dom))))
        (path-node (pop paths-nodes) (pop paths-nodes))
        (paragraph (make-paragraph)))
    ((null path-node)
     (progn (unless (paragraph-emptyp paragraph) (push paragraph paragraphs))
       (classify-paragraphs (reverse paragraphs))))
    (destructuring-bind (path node) path-node
      (unless (find (plump:tag-name node) *skipped-tags* :test #'equalp)
        ;; Extend the path.
        (add-end path (plump:tag-name node))
        ;; Start a new paragraph if need be.
        (when (find (plump:tag-name node) *paragraph-tags* :test #'equalp)
          (unless (paragraph-emptyp paragraph) ; othewise just reuse the previous object
            (push paragraph paragraphs)
            (setf paragraph (make-paragraph)))
          (setf (paragraph-path paragraph) path)
          (when (find-if (lambda (tag) (cl-ppcre:scan "^h\\d$" tag)) path)
            (setf (paragraph-headingp paragraph) t)))
        ;; Links counting.
        (when (equalp "a" (plump:tag-name node))
          (incf (paragraph-chars-count-in-links)
                (length (plump:render-text node))))
        ;; Add the text to the paragraph.
        (when (plump:textual-node-p node)
          (add-end (paragraph-text-nodes paragraph)
                   (cleaned-text (plump:text node))))
        ;; Add the children at the front of the queque (depth-first).
        (setf paths-nodes
              (append (mapcar (lambda (elem)
                                (list (copy-list path) elem))
                              (vector->list (plump:child-elements node)))
                      paths-nodes))))))

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
