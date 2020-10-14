(in-package :omnivore2)
(declaim (optimize (debug 3)))

(defpackage :spacy) ; to avoid complaining about non-existing package when loading this file
(py4cl:python-exec "import spacy")
(py4cl:import-module "spacy" :reload t)
(defparameter *spacy-nlp* (spacy::load *spacy-model-name*)) ; "not external", lol
(py4cl:python-exec "from spacy_langdetect import LanguageDetector")
(py4cl:python-method *spacy-nlp* 'add_pipe (py4cl:python-call "LanguageDetector"))

(defun spacy-doc-for (text)
  (py4cl:python-call *spacy-nlp* text))

(defun string-sequence-from-spacy-obj (obj &key (type 'list))
  (map type
       (lambda (sent) (py4cl:python-call "str" sent))
       (py4cl:python-call "list" obj)))

