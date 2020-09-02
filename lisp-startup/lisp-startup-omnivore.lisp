;;; Load our code.
(load "/root/quicklisp/setup.lisp")
(push "/lisp/" ql:*local-project-directories*)
(ql:quickload :omnivore :silent t)
(ql:quickload :command-line-arguments :silent t)

(defparameter +command-line-spec+
  '((("sites" #\s) :type string :optional t :documentation "a list of sites to include, space-separated")
    (("start-date") :type string :optional t :documentation "start date for Solr query, in its format")
    (("end-date") :type string :optional t :documentation "end date for Solr query, in its format")
    (("undated") :type boolean :optional t :documentation "allow undated documents?")))

(command-line-arguments:handle-command-line
  +command-line-spec+
  (lambda (&rest args)
    (format t "~A~%"
            (omnivore:results-formatted-as-json (apply #'omnivore:query-result-from-solr args))))
  :positional-arity 1) ;; get the query as a positional argument
