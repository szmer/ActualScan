(defpackage :omnivore (:use :cl :trivial-types :textviews)
  (:export :query-result-from-solr :results-formatted-as-json))
(in-package :omnivore)

(defparameter *debug-times* nil)
(defparameter *debug-scoring* nil)
(defparameter *debug-solr-connection* nil)

;;-(defparameter *filesystem-db-path* #p"~/therminsley/willowseed/db/")

(setf drakma:*drakma-default-external-format* :utf-8)

(defparameter *phrase-freq-threshold* 0.005) ; as ratio of number of sentences available
(defparameter *phrase-example-count* 3)

(defparameter *solr-address* "solr")
(defparameter *solr-port* 8983)
(defparameter *solr-collection* "ascan")
(defparameter *solr-total-row-limit* 800)
;; getting less seems to encourage more site diversity, although it's ambivalent by itself
(defparameter *solr-group-row-limit* 500);150)
;; a higher value can theoretically encourage longer texts (articles etc.)
(defparameter *solr-snippets-per-doc* 7)
