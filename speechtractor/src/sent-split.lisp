(in-package :speechtractor)

;;;; This file was copied on 26.01.2020 from the CL-NLP project, distributed under the following
;;;; license. I added package indications for stuff used from rutils and rutilsx and copied their
;;;; list of dot abbreviations.
;;;;
;;;; Copyright 2013-2016 Vsevolod Dyomkin
;;;; 
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;; 
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;; 
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

(defgeneric tokenize (tokenizer string)
  (:documentation
   "Tokenize STRING with TOKENIZER. Outputs 2 values:

    - list of words
    - list of spans as beg-end pairs"))

(defclass tokenizer ()
  ()
  (:documentation
   "Base class for tokenizers."))

(defmethod tokenize :around ((tokenizer tokenizer) string)
  "Pre-split text into lines and tokenize each line separately."
  (let ((offset 0)
        words spans)
    (loop :for line :in (rutils.sequence:split-sequence #\Newline string) :do
          (rutils.abbr:mv-bind (ts ss) (call-next-method tokenizer line)
            (:= words (nconc words ts)
             spans (nconc spans (mapcar (lambda (x)
                                          (rutils.pair:pair (+ (rutils.pair:lt x) offset)
                                                            (+ (rutils.pair:rt x) offset))) 
                                        ss)))
            (:+ offset (1+ (length line)))))
    (values words
            spans)))

;;; Word tokenization

(defclass regex-word-tokenizer (tokenizer)
  ((regex :accessor tokenizer-regex :initarg :regex
          :initform
          (cl-ppcre:create-scanner
           "\\w+|[!\"#$%&'*+,./:;<=>?@^`~…\\(\\)⟨⟩{}\\[\\|\\]‒–—―«»“”‘’¶-]")
          :documentation
          "A simpler variant would be [^\\s]+ —
           it doesn't split punctuation, yet sometimes it's desirable."))
  (:documentation
   "Regex-based word tokenizer."))

(defmethod tokenize ((tokenizer regex-word-tokenizer) string)
  (loop :for (beg end) :on (cl-ppcre:all-matches (tokenizer-regex tokenizer) string)
                       :by #'cddr
        :collect (rutils.array:slice string beg end) :into words
        :collect (rutils.pair:pair beg end) :into spans
        :finally (return (values words
                                 spans))))

;;; Sentence splitting

(defclass punct-sent-tokenizer (tokenizer)
  ((abbrevs-with-dot :initarg :abbrevs-with-dot
                     :initform
                     (list  "e.g." "i.e." "Mr." "Ms." "Mrs." "Dr." "St." "Rd." "Ave." "A.D."
                            "B.C." "Mt."))
   (sent-end-chars :initarg :sent-end-chars
                   :initform '(#\. #\? #\! #\… #\¶))
   (sent-post-end-chars :initarg :sent-post-end-chars
                        :initform '(#\) #\" #\' #\» #\”)))
  (:documentation
   "Basic tokenizer for sentence splitting."))

(defmethod tokenize ((tokenizer punct-sent-tokenizer) string)
  (rutilsx.bind:with
    ((words word-spans
            (tokenize (make-instance 'regex-word-tokenizer :regex "[^\\s]+")
                      (substitute #\¶ #\Newline string)))
     ((sent-end-chars sent-post-end-chars abbrevs-with-dot) rutilsx.bind:@ tokenizer)
     (beg 0)
     (sents nil)
     (spans nil))
    (loop :for (word . ws) :on words
          :for (span . ss) :on word-spans :do
          (let ((last-char (char word (1- (length word)))))
            (when (or (null ws)
                      (and (cond ((member last-char sent-end-chars)
                                  (not (and (char= #\. last-char)
                                            (member word abbrevs-with-dot
                                                    :test 'string=))))
                                 ((member last-char sent-post-end-chars)
                                  (and (> (length word) 1)
                                       (member (char word (- (length word) 2))
                                               sent-end-chars))))
                           (or ;; normal case
                             (and (not (lower-case-p (rutilsx.generic:? ws 0 0)))
                                  ;; not name shortening
                                  (not (and (= 2 (length word))
                                            (char= #\. last-char)
                                            (upper-case-p (char word 0)))))
                             ;; all lower case
                             (every (lambda (w) (notany 'upper-case-p w)) words)
                             ;; all caps
                             (every (lambda (w) (notany 'lower-case-p w)) words))))
              (push (rutils.array:slice string beg (rutils.pair:rt span)) sents)
              (push (rutils.pair:pair beg (rutils.pair:rt span)) spans)
              (when ws
                (:= beg (rutilsx.generic:? ss 0 0))))))
    (values (reverse sents)
            (reverse spans))))
