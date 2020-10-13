(in-package :omnivore2)

(defun context-for-period-alist (period-alist &key (minimum-size *minimum-context-size*)
                                                (maximum-size *maximum-context-size*))
  (let ((http-query (format nil
                            (concatenate 'string
                                         "http://~A:~A/solr/~A/select?q=*:*"
                                         (solr-query-field-from-period-alist period-alist)
                                         "&"
                                         (solr-boost-field-from-period-alist period-alist)
                                         ;; Set how many rows we want to get.
                                         "&rows=~A")
                            *solr-address* *solr-port* *solr-collection* maximum-size))
        (response (babel:octets-to-string
                   (drakma:http-request http-query
                                        ;; this is needed to preserve the encoded space in
                                        ;; sort clause
                                        ;; see https://github.com/edicl/drakma/issues/78
                                        :preserve-uri t :external-format-in :utf-8)))
        (response-json (cl-json:decode-json-from-string response))
        (json-docs (or (cdr (assoc :docs
                                   (or (cdr (assoc :response response-json))
                                       (error (format nil "No response field in ~A~%"
                                                      response)))))
                       (error (format nil "No response->docs field in ~A~%"
                                      response)))))
    (when (>= (length json-docs) minimum-size)
      json-docs)))
