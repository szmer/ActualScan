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

(defmacro first-beginning-p (text)
  "Ensure that the text is at the beginning of the first document's text."
  `(equalp ,text (subseq (cdr (assoc :text (first parsed-docs)))
                         0 (min (length ,text)
                                (length (cdr (assoc :text (first parsed-docs))))))))

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
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
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
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "Ennius" (cdr (assoc :author (first parsed-docs)))))
      ;; Ensure no junk at the beginning
      (is (first-beginning-p "Hey Gents"))
      (is (equalp "/threads/wool-blend-jeans-five-pocket-pants.656128/post-10023303"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (equalp "circumspice" (cdr (assoc :author (second parsed-docs)))))
      (is (equalp "2020:01:06T15:53:00Z" (cdr (assoc :date--post (second parsed-docs)))))
      (is (equalp (format nil "@Gus may have some recommendations.~%I think something in this vein is his travel pent jam")
                  (cdr (assoc :text (second parsed-docs)))))
      (is (equalp (format nil "Not finding much in search, unfortunately")
                  (cdr (assoc :text (fourth parsed-docs)))))
      (is (equalp "/threads/wool-blend-jeans-five-pocket-pants.656128/post-10027224"
                  (cdr (assoc :url (first (last parsed-docs))))))
      ;; Omit the cookie notice.
      (is (not (search "Privacy Policy" (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "register to reply here" (cdr (assoc :text (car (last parsed-docs)))))))
      (is (search "Thanks Sir!" (cdr (assoc :text (car (last parsed-docs)))))))))

(deftest x-welookfab-wordpress ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "welookfab-wordpress.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 22 (length parsed-docs)))
      ;; The basic check for lost posts.
      (is (eq nil
              (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       ;; KLUDGE ? remove "Beautiful mittens, too.", "Love it! Total Diana",
                       ;; "You can't go wrong"
                       (remove-if (lambda (doc) (find (cdr (assoc :author doc))
                                                      '("JAileen" "rachylou" "Cee")
                                                      :test #'equalp))
                                  parsed-docs))))
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "Diana" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp (speechtractor::date-solr-str "11 months ago")
                  (cdr (assoc :date--post (first parsed-docs)))))
      (is (equalp "https://youlookfab.com/welookfab/topic/angie-challenge-day-1-ffbo-handknit-sweater-jeans-combat-boots#post-2006057"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (first-beginning-p "Today's Angie Challenge entry is a FFBO, featuring my favorite sweater, perfect jeans, and combat boots."))
      (is (equalp "Suz" (cdr (assoc :author (second parsed-docs)))))
      (is (equalp (speechtractor::date-solr-str "11 months ago")
                  (cdr (assoc :date--post (second parsed-docs)))))
      ;; NOTE the space in Diana , is because of text node concatenation. It could potentially not
      ;; add that before a punctuation mark?
      (is (equalp (format nil "Diana , you look amazing.~%SOOOOO cozy all bundled up.~%Of course your first \"me\" outfit had to include one of your beautiful hand knits!~%That sweater is so texturally rich.~%Love both outfits on you.")
                  (cdr (assoc :text (second parsed-docs)))))
      (is (equalp "https://youlookfab.com/welookfab/topic/angie-challenge-day-1-ffbo-handknit-sweater-jeans-combat-boots#post-2006846"
                  (cdr (assoc :url (car (last parsed-docs))))))
      (is (search "Unfortunately the Liberty jeans can't be patched."
                  (cdr (assoc :text (car (last parsed-docs))))))
      (is (not (search "Not a member?"
                  (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "Rights Reserved"
                  (cdr (assoc :text (car (last parsed-docs))))))))))

(deftest x-thestudentroom ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "thestudentroom.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 9 (length parsed-docs)))
      ;; The basic check for lost posts.
      (is (eq nil
              (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       ;; KLUDGE ? remove "New Look have quite cheap jeans"
                       (remove-if (lambda (doc) (find (cdr (assoc :author doc))
                                                      '("SlightlySummer")
                                                      :test #'equalp))
                                  parsed-docs))))
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "RG250" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp (speechtractor::date-solr-str "7 months ago")
                  (cdr (assoc :date--post (first parsed-docs)))))
      (is (equalp "#post83753960"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (first-beginning-p "I have always shopped at Primark for jeans but they don't last very long"))
      (is (equalp "#post83897350"
                  (cdr (assoc :url (car (last parsed-docs))))))
      (is (search "Their jeans are amazing."
                  (cdr (assoc :text (car (last parsed-docs))))))
      (is (not (search "We have a brilliant team of more than 60 Support Team members"
                  (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "Tell us a little about yourself to get started."
                  (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "Have you ever signed up for an open day and then not gone to it?"
                  (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "are trading names of The Student Room Group Ltd."
                  (cdr (assoc :text (car (last parsed-docs))))))))))
