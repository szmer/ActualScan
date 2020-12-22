(in-package :speechtractor-test)

(deftest http ()
  (when speechtractor::*server-running-p*
    (is (typep speechtractor::*server* 'hunchentoot:acceptor))
    (let* ((response (drakma:http-request
                         (format nil
                                 (concatenate 'string
                                              "https://127.0.0.1:~A/api/v01/interpret"
                                              "?sourcetype=test&emptyurl=1&html=~A")
                                 speechtractor::*http-port*
                                 (drakma:url-encode
                                   "<body><p author='Sophie'>My very interesting and completely realistic example with lots of words</p></body>"
                                   :utf-8))
                         :force-ssl t
                         :basic-authorization
                         (if (sb-ext:posix-getenv "SPEECHTRACTOR_USER")
                             (list (sb-ext:posix-getenv "SPEECHTRACTOR_USER")
                                   (sb-ext:posix-getenv "SPEECHTRACTOR_PASSWORD"))
                             (list "stractor" "stractor"))
                         ;; don't mess with our encoding
                         :preserve-uri t))
           (response-json (cl-json:decode-json-from-string response)))
      (is (= 1 (length response-json)))
      (is (equalp "Sophie" (cdr (assoc :author (first response-json)))))
      (is (equalp "My very interesting and completely realistic example with lots of words"
                  (cdr (assoc :text (first response-json))))))))
