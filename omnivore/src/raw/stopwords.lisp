(in-package :omnivore)

;;;; This list was retrieved and code ported from SpaCy repo master branch on 12.01.2020.
;;;;
;;;; SpaCy's license:
;;;; The MIT License (MIT)
;;;;
;;;; Copyright (C) 2016-2019 ExplosionAI GmbH, 2016 spaCy GmbH, 2015 Matthew Honnibal
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;;; THE SOFTWARE.
(defparameter *stopwords*
  (list-as-hash-set
    (list
      "a" "about" "above" "across" "after" "afterwards" "again" "against" "all" "almost" "alone"
      "along" "already" "also" "although" "always" "am" "among" "amongst" "amount" "an" "and"
      "another" "any" "anyhow" "anyone" "anything" "anyway" "anywhere" "are" "around" "as" "at"
      "back" "be" "became" "because" "become" "becomes" "becoming" "been" "before" "beforehand"
      "behind" "being" "below" "beside" "besides" "between" "beyond" "both" "bottom" "but" "by"
      "call" "can" "cannot" "ca" "could" "did" "do" "does" "doing" "done" "down" "due" "during"
      "each" "eight" "either" "eleven" "else" "elsewhere" "empty" "enough" "even" "ever" "every"
      "everyone" "everything" "everywhere" "except" "few" "fifteen" "fifty" "first" "five" "for"
      "former" "formerly" "forty" "four" "from" "front" "full" "further" "get" "give" "go" "had"
      "has" "have" "he" "hence" "her" "here" "hereafter" "hereby" "herein" "hereupon" "hers"
      "herself" "him" "himself" "his" "how" "however" "hundred" "i" "if" "in" "indeed" "into" "is"
      "it" "its" "itself" "keep" "last" "latter" "latterly" "least" "less" "just" "made" "make"
      "many" "may" "me" "meanwhile" "might" "mine" "more" "moreover" "most" "mostly" "move" "much"
      "must" "my" "myself" "name" "namely" "neither" "never" "nevertheless" "next" "nine" "no"
      "nobody" "none" "noone" "nor" "not" "nothing" "now" "nowhere" "of" "off" "often" "on" "once"
      "one" "only" "onto" "or" "other" "others" "otherwise" "our" "ours" "ourselves" "out" "over"
      "own" "part" "per" "perhaps" "please" "put" "quite" "rather" "re" "really" "regarding" "same"
      "say" "see" "seem" "seemed" "seeming" "seems" "serious" "several" "she" "should" "show" "side"
      "since" "six" "sixty" "so" "some" "somehow" "someone" "something" "sometime" "sometimes"
      "somewhere" "still" "such" "take" "ten" "than" "that" "the" "their" "them" "themselves" "then"
      "thence" "there" "thereafter" "thereby" "therefore" "therein" "thereupon" "these" "they"
      "third" "this" "those" "though" "three" "through" "throughout" "thru" "thus" "to" "together"
      "too" "top" "toward" "towards" "twelve" "twenty" "two" "under" "until" "up" "unless" "upon"
      "us" "used" "using" "various" "very" "very" "via" "was" "we" "well" "were" "what" "whatever"
      "when" "whence" "whenever" "where" "whereafter" "whereas" "whereby" "wherein" "whereupon"
      "wherever" "whether" "which" "while" "whither" "who" "whoever" "whole" "whom" "whose" "why"
      "will" "with" "within" "without" "would" "yet" "you" "your" "yours" "yourself" "yourselves"
      ;; possible in CoNLLU trees
      "-pron-")))

(let ((contractions (list "n't" "'d" "'ll" "'m" "'re" "'s" "'ve")))
  (dolist (item contractions)
    (setf (gethash item *stopwords*) t))
  (dolist (apostrophe (list "‘" "’"))
    (dolist (item contractions)
      (setf (gethash (cl-strings:replace-all item "'" apostrophe) *stopwords*) t))))

;;;; (end of SpaCy code)

(defparameter *alphabetic*
  (list-as-hash-set
    (list
      #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\w #\v
      #\x #\y #\z)))

(defun alphabetic-present-p (string)
  (truep (find-if (lambda (char) (gethash char *alphabetic*)) string)))

(defun stopwordp (word)
  "The word needs to be already lowercased."
  (or (gethash word *stopwords*)
      (not (alphabetic-present-p word))))
(assert (not (stopwordp "aasadsdf")))
(assert (stopwordp "was"))
(assert (stopwordp "()"))
