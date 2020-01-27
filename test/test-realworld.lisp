(in-package :speechtractor-test)

(deftest x-redflagdeals-phpbb ()
  (when speechtractor::*server-running-p*
    (let* ((response
             (drakma:http-request
               (format nil
                       (concatenate 'string
                                    "http://127.0.0.1:~A/api/v01/interpret")
                       speechtractor::*http-port*)
               :method :post
               :parameters
               (list '("sourcetype" . "forums")
                     (cons "html" (uiop:read-file-string
                                    (asdf:system-relative-pathname
                                      'speechtractor "test/pages/redflagdeals-phpbb.html"))))))
           (parsed-docs (cl-json:decode-json-from-string response)))
      (is (= 12 (length parsed-docs)))
      (is (equalp "unshavenyak" (cdr (assoc :author (first parsed-docs)))))
      ;; That's how cl-json re-reads date_post
      (is (equalp "2020:01:10T18:03:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (equalp "#p32010571" (cdr (assoc :url (first parsed-docs)))))
      (is (equalp (format nil "Your fashion pet peeves~%~%Alright fashionistas.~%What is fashion faux pas drives you crazy?~%I will get the ball rolling: men who wear white crew necks under their dress shirts and leave the neck open.~%It's so sloppy.")
                  (cdr (assoc :text (first parsed-docs)))))
      (is (equalp "smartie" (cdr (assoc :author (nth 11 parsed-docs)))))
      (is (equalp "2020:01:14T21:52:00Z" (cdr (assoc :date--post (nth 11 parsed-docs)))))
      (is (equalp "#p32027817" (cdr (assoc :url (nth 11 parsed-docs)))))
      (is (search "Becks wrote:" (cdr (assoc :text (nth 11 parsed-docs)))))
      (is (search (format nil "I have to ask which area do you live?~%Seems a lot artists there")
                  (cdr (assoc :text (nth 11 parsed-docs))))))))
