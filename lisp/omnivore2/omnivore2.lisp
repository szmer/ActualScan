(defpackage :omnivore2 (:use :cl))
(in-package :omnivore2)

;;; Whether to run the server.
(defparameter *server-running-p* t)
(defparameter *http-port* (if (sb-ext:posix-getenv "OMNIVORE2_PORT")
                              (cl-strings:parse-number (sb-ext:posix-getenv "OMNIVORE2_PORT"))
                              6823))
;; Silence hunchentoot reporting.
(defparameter *server-silentp* t)
;; Should the hunchentoot REPL enter the interpreter debugger on error.
(defparameter *server-enter-debug-p* t)
;; Should we print logs for incoming HTTP requests.
(defparameter *log-requests-p* t)

;; Solr connection.
(defparameter *solr-address* "solr")
(defparameter *solr-port* 8983)
(defparameter *solr-collection* "ascan")

;;; Linguistic modeling.
(defparameter *spacy-model-name* "en_core_web_sm")
;; These length is in words. If the sentence is *good-minimal-period-length* or longer, we won't
;; cause an attempt for forming a longer period.
(defparameter *good-minimal-period-length* 30)
;; If the period would be longer than *good-maximal-period-length*, we won't create it.
(defparameter *good-maximal-period-length* 50)
(defparameter *common-ngrams-count-for-merging* 7)

(defparameter *stationary-analytic-funs* '(#'add-average-word-length-for-period))
(defparameter *contextual-analytic-funs* '(#'add-average-tf-idf-for-period
                                           #'add-sentences-length-deviation-for-period))

(defparameter *minimum-context-size* 200)
(defparameter *maximum-context-size* 800)

;;; Setup py4cl (we have to do it in a separate file like this for compilation to actually kill
;;; the default python[2])
(setf py4cl:*python-command* "python3")
(py4cl:python-stop) ;; force it to reload with the good executable
