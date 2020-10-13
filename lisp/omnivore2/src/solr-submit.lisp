(in-package :omnivore2)

(defun submit-docs-to-solr (docs)
  (multiple-value-bind (response status-code)
      (drakma:http-request (format nil "http://~A:~A/solr/~A/update"
                                   *solr-address* *solr-port* *solr-collection*)
                           :content-type "application/json"
                           :content (cl-json:encode-json-to-string docs))
    status-code))
