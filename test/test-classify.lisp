(in-package :speechtractor-test)

;;;; Basic classification tests.
;;;; These basic tests were 
;;;;
;;;; Copyright (c) 2011, Jan Pomikalek <jan.pomikalek@gmail.com> Copyright (c) 2013, Michal Belica
;;;; 
;;;; All rights reserved.
;;;; 
;;;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;;; 
;;;; Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;;; Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; Ported by Szymon Rutkowski.

(defun make-test-paragraph (text)
  (let ((paragraph (speechtractor::make-paragraph)))
    (setf (speechtractor::paragraph-path paragraph) (list "body" "p"))
    (setf (speechtractor::paragraph-text-nodes paragraph) (list text))
    paragraph))

(deftest basic-link-density ()
  "Link density."
(let ((paragraphs
            (mapcar
              (lambda (text count-in-links)
                (let ((paragraph (make-test-paragraph text)))
                  (setf (speechtractor::paragraph-chars-count-in-links paragraph) count-in-links)
                  paragraph))
              (list (make-string 20 :initial-element #\a)
                    (make-string 20 :initial-element #\a)
                    (make-string 80 :initial-element #\a)
                    (make-string 80 :initial-element #\a)
                    (make-string 80 :initial-element #\a))
              '(0 20 40 39 41))))
      (speechtractor::classify-paragraphs-basic paragraphs
        :classification-settings (alexandria:alist-hash-table (list (cons :max-link-density 0.5))))
      (is (eq (speechtractor::paragraph-classification (elt paragraphs 0)) :short))
      (is (eq (speechtractor::paragraph-classification (elt paragraphs 1)) :bad))
      (is (eq (speechtractor::paragraph-classification (elt paragraphs 2)) :bad))
      (is (eq (speechtractor::paragraph-classification (elt paragraphs 3)) :bad))
      (is (eq (speechtractor::paragraph-classification (elt paragraphs 4)) :bad))))

(deftest basic-low-length ()
  "Low length."
  (let ((paragraphs
          (mapcar
            (lambda (text count-in-links)
              (let ((paragraph (make-test-paragraph text)))
                (setf (speechtractor::paragraph-chars-count-in-links paragraph) count-in-links)
                paragraph))
            (list (make-string 40 :initial-element #\a)
                  (make-string 40 :initial-element #\a))
            '(0 20))))
    (speechtractor::classify-paragraphs-basic paragraphs
                                              :classification-settings (alexandria:alist-hash-table (list (cons :length-low 1000))))
    (is (eq (speechtractor::paragraph-classification (elt paragraphs 0)) :short))
    (is (eq (speechtractor::paragraph-classification (elt paragraphs 1)) :bad))))

(deftest basic-stopwords-high-len ()
  "Stopwords, high length."
  (let ((paragraphs
          (mapcar
            #'make-test-paragraph
            (list "a 1 2 3 4 5 6 7 8 9"
                  "a a 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9"))))
    (speechtractor::classify-paragraphs-basic
      paragraphs
      :classification-settings
      (alexandria:alist-hash-table (list (cons :max-link-density 1)
                                         (cons :length-low 0)
                                         (cons :length-high 20)
                                         (cons :stopwords-high 0))))
    (is (eq (speechtractor::paragraph-classification (elt paragraphs 0)) :near-good))
    (is (eq (speechtractor::paragraph-classification (elt paragraphs 1)) :good))))

(deftest basic-stopwords-low-len ()
  "Stopwords, low length."
  (let ((paragraphs
            (mapcar
              #'make-test-paragraph
              (list "a a a a a 2 3 4 5 6 7 8 9"
                    "a a 2 3 4 5 6 7 8 9"
                    "a 2 3 4 5 6 7 8 9"))))
      (speechtractor::classify-paragraphs-basic paragraphs
        :classification-settings
        (alexandria:alist-hash-table (list (cons :max-link-density 1)
                                           (cons :length-low 0)
                                           (cons :stopwords-high 1)
                                           ;; for some float reason fails with 0.2
                                           (cons :stopwords-low 1/5))))
      (is (eq (speechtractor::paragraph-classification (elt paragraphs 0)) :near-good))
      (is (eq (speechtractor::paragraph-classification (elt paragraphs 1)) :near-good))
      (is (eq (speechtractor::paragraph-classification (elt paragraphs 2)) :bad))))

(deftest full-classification ()
  "Html-document-data shouldn't leave :short or :near-good stuff."
  (let* ((html
              "<body><p>a a a a a</p><p>12</p><p>2 3 4 5 6 7 8 9</p><p>a a 2 3 4 5 6 7 8 9<p></body>")
         (settings (alexandria:alist-hash-table (list (cons :max-link-density 1)
                                                          (cons :length-low 3)
                                                          (cons :length-high 6)
                                                          (cons :stopwords-high 1/5)
                                                          (cons :stopwords-low 1/10))))
         (paragraphs (speechtractor::html-document-data
                       html nil
                       :classification-settings settings))
         (initial-classification
           (speechtractor::classify-paragraphs-basic
             (mapcar
               #'make-test-paragraph
               (list "a a a a a" "12" "2 3 4 5 6 7 8 9" "a a 2 3 4 5 6 7 8 9"))
             :classification-settings settings)))
    (is (eq (speechtractor::paragraph-classification (elt initial-classification 0)) :good))
    (is (eq (speechtractor::paragraph-classification (elt initial-classification 1)) :short))
    (is (eq (speechtractor::paragraph-classification (elt initial-classification 2)) :bad))
    (is (eq (speechtractor::paragraph-classification (elt initial-classification 3)) :near-good))
    (is (= (length paragraphs) 4))
    (is (eq (speechtractor::paragraph-classification (elt paragraphs 0)) :good))
    (is (eq (speechtractor::paragraph-classification (elt paragraphs 1)) :bad))
    (is (eq (speechtractor::paragraph-classification (elt paragraphs 2)) :bad))
    (is (eq (speechtractor::paragraph-classification (elt paragraphs 3)) :bad))))
