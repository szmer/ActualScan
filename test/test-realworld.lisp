(in-package :speechtractor-test)

(defun request-test-page (file-name source-type)
  (drakma:http-request
               (format nil
                       (concatenate 'string
                                    "http://127.0.0.1:~A/api/v01/interpret")
                       speechtractor::*http-port*)
               :method :post
               :parameters
               (list (cons "sourcetype" source-type)
                     (cons "html" (uiop:read-file-string
                                    (asdf:system-relative-pathname
                                      'speechtractor
                                      (format nil "test/pages/~A" file-name)))))))

(deftest x-redflagdeals-phpbb ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "redflagdeals-phpbb.html" "forums"))
           ;; If it fails, we may want to format t the response to debug
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 12 (length parsed-docs)))
      ;; The basic check for lost posts.
      (is (eq nil
              (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       ;; KLUDGE ? we skip the ultra-short "Crocs and socks." comment
                       (remove-if (lambda (doc) (equalp "uber_shnitz" (cdr (assoc :author doc))))
                                  parsed-docs))))
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

(deftest x-styleforum-xenforo ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "styleforum-xenforo.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 6 (length parsed-docs)))
      ;; The basic check for lost posts.
      (is (eq nil
              (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       parsed-docs)))
      (is (equalp "Ennius" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "circumspice" (cdr (assoc :author (second parsed-docs)))))
      (is (equalp "2020:01:06T15:53:00Z" (cdr (assoc :date--post (second parsed-docs)))))
      (is (equalp (format nil "@Gus may have some recommendations.~%I think something in this vein is his travel pent jam")
                  (cdr (assoc :text (second parsed-docs)))))
      (is (equalp (format nil "Not finding much in search, unfortunately")
                  (cdr (assoc :text (fourth parsed-docs)))))
      ;; Omit the cookie notice.
      (is (not (search "Privacy Policy" (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "register to reply here" (cdr (assoc :text (car (last parsed-docs)))))))
      (is (search "Thanks Sir!" (cdr (assoc :text (car (last parsed-docs)))))))))
