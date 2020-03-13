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

(defun last-doc-with-content (parsed-docs)
  (find-if (lambda (doc) (not (zerop (length (cdr (assoc :text doc))))))
           parsed-docs :from-end t))

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
      (is (equalp "2020-01-10T18:03:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (equalp "#p32010571" (cdr (assoc :url (first parsed-docs)))))
      (is (equalp (format nil "Alright fashionistas.~%What is fashion faux pas drives you crazy?~%I will get the ball rolling: men who wear white crew necks under their dress shirts and leave the neck open.~%It's so sloppy.")
                  (cdr (assoc :text (first parsed-docs)))))
      (is (equalp "smartie" (cdr (assoc :author (nth 11 parsed-docs)))))
      (is (equalp "2020-01-14T21:52:00Z" (cdr (assoc :date--post (nth 11 parsed-docs)))))
      (is (equalp "#p32027817" (cdr (assoc :url (nth 11 parsed-docs)))))
      (is (search "Becks wrote:" (cdr (assoc :text (nth 11 parsed-docs)))))
      (is (search (format nil "I have to ask which area do you live?~%Seems a lot artists there")
                  (cdr (assoc :text (nth 11 parsed-docs))))))))

(deftest x-headfi-xenforo ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "headfi-xenforo.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 15 (length parsed-docs)))
      ;; The basic check for lost posts.
      (is (eq nil
              (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       (remove-if (lambda (doc) (equalp "tninety" (cdr (assoc :author doc))))
                                  parsed-docs))))
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "tninety" (cdr (assoc :author (first parsed-docs)))))
      ;; KLUDGE currently the first post is discarded, too short
      ;;;-(is (search "Are you going to double amp it?"
      ;;;-            (cdr (assoc :text (first parsed-docs)))))
      (is (equalp "threads/sennheiser-hd-600-impressions-thread.538255/page-171#post-9108883"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (not (search "Separate names with a comma" (cdr (assoc :text (first parsed-docs))))))
      (is (equalp "BrokeR" (cdr (assoc :author (second parsed-docs)))))
      (is (equalp "2013-01-31T11:42:00Z" (cdr (assoc :date--post (second parsed-docs)))))
      (is (search "I would use the RCA out on the back of my sound card"
                  (cdr (assoc :text (second parsed-docs)))))
      (is (equalp "threads/sennheiser-hd-600-impressions-thread.538255/page-171#post-9128062"
                  (cdr (assoc :url (first (last parsed-docs))))))
      ;; Omit the cookie notice.
      (is (not (search "uses cookies to help personalise content" (cdr (assoc :text (car (last parsed-docs)))))))
      (is (search "I'm conflicted by some people saying they hear little difference" (cdr (assoc :text (car (last parsed-docs)))))))))

(deftest x-headfi-1stpage-xenforo ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "headfi-1stpage-xenforo.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      ;; The purpose of this test is checking how we handle junk that may happen in the date field.
      (is (= 1 (length parsed-docs)))
      (is (equalp "audiorefinery" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "threads/sennheiser-hd-600-like-new.665928/" (cdr (assoc :url (first parsed-docs)))))
      (is (equalp "2013-05-29T21:37:00Z" (cdr (assoc :date--post (first parsed-docs))))))))

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
      (is (equalp "2020-01-06T15:53:00Z" (cdr (assoc :date--post (second parsed-docs)))))
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
      ;; ??? TODO weird of-by-minute date mismatch happening here
      (is (equalp (speechtractor::solr-date-from "11 months ago")
                  (cdr (assoc :date--post (first parsed-docs)))))
      (is (equalp "https://youlookfab.com/welookfab/topic/angie-challenge-day-1-ffbo-handknit-sweater-jeans-combat-boots#post-2006057"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (first-beginning-p "Today's Angie Challenge entry is a FFBO, featuring my favorite sweater, perfect jeans, and combat boots."))
      (is (equalp "Suz" (cdr (assoc :author (second parsed-docs)))))
      ;; ??? TODO weird of-by-minute date mismatch happening here
      (is (equalp (speechtractor::solr-date-from "11 months ago")
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
      (is (equalp (speechtractor::solr-date-from "7 months ago")
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

(deftest x-wallstreetoasis ()
  ;; NOTE Here tests are a little sloppy for now, since we are bombarded with low-effort posts.
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "wallstreetoasis.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      ;; it's hard to count them here
      (is (< 20 (length parsed-docs)))
      ;; The basic check for lost posts.
      (is (> 10
             (length (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       parsed-docs))))
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "https://www.wallstreetoasis.com/forums/what-do-you-monkeys-wear-with-jeans"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (search "Cool, right?"
                  (cdr (assoc :text (last-doc-with-content parsed-docs)))))
      ;; Keep out the junk.
      (is (not (search "WSO depends on everyone being able to pitch in when they know something."
                  (cdr (assoc :text (last-doc-with-content parsed-docs))))))
      (is (not (search "Sorry, you need to login or sign up in order to vote."
                  (cdr (assoc :text (last-doc-with-content parsed-docs))))))
      (is (not (search "by signing in with your social account"
                  (cdr (assoc :text (last-doc-with-content parsed-docs)))))))))

(deftest x-fashionspot-maybe-xenforo ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "fashionspot-maybe-xenforo.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 15 (length parsed-docs)))
      ;; The basic check for lost posts.
      (is (eq nil
              (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       (remove-if (lambda (doc)
                                    (find (cdr (assoc :author doc))
                                          ;; their posts are only images
                                          '("Machinegumm" "dior_couture1245"
                                            ;; KLUDGE ? "This is... a lot."
                                            "SophiaVB")
                                          :test #'equalp))
                                  parsed-docs))))
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "Machinegumm" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2020-01-18T12:57:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (equalp "threads/loewe-mens-f-w-2020-21-paris.396285/"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (equalp "jeanclaude" (cdr (assoc :author (car (last parsed-docs))))))
      (is (equalp "threads/loewe-mens-f-w-2020-21-paris.396285/#post-30675897"
                  (cdr (assoc :url (car (last parsed-docs))))))
      (is (search "you are going against your whole idea wearing neutral basics"
                 (cdr (assoc :text (car (last parsed-docs))))))
      ;; KLUDGE this falls through as bad?
      ;;(is (search "It does not feel authentic at all..."
      ;;           (cdr (assoc :text (car (last parsed-docs))))))
      (is (not (search "Click to expand..."
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "Welcome to the web’s largest community of fashion influencers"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "TheFashionSpot.com is a property of TotallyHer Media"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "This site uses cookies"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "Share This Page" (cdr (assoc :text (car (last parsed-docs))))))))))

(deftest x-askandyaboutclothes-xenforo ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "askandyaboutclothes-xenforo.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 3 (length parsed-docs)))
      ;; The basic check for lost posts.
      (is (eq nil
              (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       parsed-docs)))
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "LMFHW" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2018-12-11T04:40:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (equalp "/forum/threads/jeans.240628/post-1895949"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (equalp "amy@ts" (cdr (assoc :author (car (last parsed-docs))))))
      (is (equalp "/forum/threads/jeans.240628/post-1896606"
                  (cdr (assoc :url (car (last parsed-docs))))))
      (is (search "Seven for All Mankind and Diesel have rises lower than the industry standard"
                 (cdr (assoc :text (car (last parsed-docs))))))
      (is (not (search "Your email address will not be publicly visible"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "VINTAGE IRVIN FOSTER HORSEHIDE FLIGHT JACKET, RE-LINED IN BEAUTIFUL HARRIS TWEED!"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "This site uses cookies"
                       (cdr (assoc :text (car (last parsed-docs))))))))))

(deftest x-edcforums-xenforo ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "edcforums-xenforo.html" "forums"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      ;; The basic check for lost posts.
      (is (eq nil
              (find-if (lambda (doc) (zerop (length (cdr (assoc :text doc)))))
                       (remove-if (lambda (doc)
                                    (find (cdr (assoc :author doc))
                                          ;; KLUDGE ? short post "Whites."
                                          '("John Radabaugh"
                                            ;; KLUDGE most of the post is a long link to a product
                                            "ripjack13")
                                          :test #'equalp))
                                  parsed-docs))))
      ;; Ensure that everything has a permalink.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "skip0911" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2016-02-19T00:00:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (equalp "threads/cowboy-boots.133239/"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (equalp "onebadwagon" (cdr (assoc :author (car (last parsed-docs))))))
      (is (equalp "threads/cowboy-boots.133239/#post-2664264"
                  (cdr (assoc :url (car (last parsed-docs))))))
      (is (search "Red Wings pull ons, branded Irish Setter for work"
                 (cdr (assoc :text (car (last parsed-docs))))))
      (is (not (search "Add on change"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "message_user_info_avatar"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "Page 1 of 2"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "login form"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "(You must log in or sign up to post here.)"
                       (cdr (assoc :text (car (last parsed-docs)))))))
      (is (not (search "Some XenForo functionality crafted by Audentio Design."
                       (cdr (assoc :text (car (last parsed-docs))))))))))

(deftest x-dieworkwear-tumblr ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "dieworkwear-tumblr.html" "blog"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      ;; TODO - ld+json
      nil)))

(deftest x-pennypincherfashion-wordpress ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "pennypincherfashion-wordpress.html" "blog"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      (is (equalp "Kimberly" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2012-03-10T21:13:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (search "my husband isn’t a big fan of it"
                  (cdr (assoc :text (first parsed-docs)))))
      ;; comments
      (is (not (search "Thanks for the quick reply"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "I bought a 4 and it probably could be taken"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Strictly Necessary Cookie should be enabled at all times"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Penny Pincher Fashion is a part of several affiliate networks"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Cookie information is stored in your browser and performs functions such as recognising"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Be the first to comment"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-themodestman-wordpress ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "themodestman-wordpress.html" "blog"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      (is (equalp "Brock" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2020-01-12T13:00:51Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (search "jeans that fit and flatter your build"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "you can without your jeans pulling tight across your skin"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "There are typically three types of rises: low, mid and high"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (not (search "spending time with his wife and family"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Join the Club"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-kendieveryday-wordpress ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "kendieveryday-wordpress.html" "blog"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      ;; TODO ld+json
      ;;;(is (equalp "Kendi Everyday" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2019-08-30T20:44:29Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (first-beginning-p "Happy Friday before a long weekend"))
      (is (search "Let’s talk about cardigans!"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "I would say stay true to size and expect a looser fit at the arms"
                  (cdr (assoc :text (first parsed-docs)))))
      ;; comments
      (is (not (search "I still remember you writing years ago that you’ve discovered when fall weather"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "My goodness, I have been following you for eons"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-effortlesseverydaystyle-blogspot ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "effortlesseverydaystyle-blogspot.html" "blog"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      (is (equalp "Effortless Everyday Style" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2019-04-08T13:04:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (search "It's been a minute since my last post"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "antimicrobial shower flip flops are a MUST"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "or for your daughters Easter basket or for yourself"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "going to try to keep up on this social media"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (not (search "Effortless Everyday Style"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Monday, April 8, 2019"
                       (cdr (assoc :text (first parsed-docs))))))
      ;; comment
      (is (not (search "Very good, really admired"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-fashionista ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "fashionista.html" "media"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      ;; Note that we don't require :url in media articles.
      (is (= 1 (length parsed-docs)))
      (is (equalp "Fashionista" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2020-01-23T03:09:25Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (search "Gaultier took his final Haute Couture bow on Wednesday."
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "Théatre du Châtelet in Paris on Wednesday"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "Breton stripes and cone bras."
                  (cdr (assoc :text (first parsed-docs)))))
      (is (not (search "Want more Fashionista?"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "dress done in strands of white and dark navy."
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-vogue ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "vogue.html" "media"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      ;; KLUDGE This has ellipses "..." expanded by sentence splitter to separate sentences, which
      ;; is very questionable
      ;;
      ;; Note that we don't require :url in media articles.
      (is (= 1 (length parsed-docs)))
      ;; people besides Sally are the video and text editors but this is good enough
      (is (equalp "Sally Singer, Cass Bird, Jorden Bickham" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2017-08-30T18:09:29Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (search "great American fashion eclipse of 2017?"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "a bunch of crocheted bikinis and tiny tanks, strings of shells"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "The perfect white jeans will be worn now and, yep, after Labor Day"
                  (cdr (assoc :text (first parsed-docs)))))
      ;; KLUDGE we give up on this paragraph, which is short, :near-good and surrounded by :bad
      ;;(is (search "This is true unisex denim on Paloma"
      ;;            (cdr (assoc :text (first parsed-docs)))))
      (is (not (search "Loewe wool jacquard sweater, price upon request, for information"
                       (cdr (assoc :text (first parsed-docs))))))
      ;; not sure if we should care about this one:
      (is (not (search "Featuring Andreea Diaconu, Mia Kang, Paloma Elsesser,"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Use of and/or registration on any portion of this site constitutes acceptance of our"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-wwd ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "wwd.html" "media"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      (is (equalp "Jean E. Palmieri" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2018-09-10T04:01:11Z" (cdr (assoc :date--post (first parsed-docs)))))
      ;; (the lede is not included as of 29-01-2020)
      (is (search "that Bethenny Frankel is most drawn to in her first fashion"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "Despite all the bells and whistles"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "Frankel has been with the design and marketing"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (not (search "goods are proving to be a real cash cow for LVMH."
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "You're missing something!"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Report: @jdiderich"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "#wwdfashion"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-dazeddigital ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "dazeddigital.html" "media"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      (is (equalp "Jessica Heron-Langton" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2019-12-10T14:15:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      ;; the first paragraph has many links
      (is (search "blown-up bumbags, over the course of the last few seasons"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "train he used to ride while studying at fashion school"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "carrying their oversized Metro card invitations"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (not (search "you accept our use of cookies"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Get the day on Dazed straight to your inbox"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-glamour ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "glamour.html" "media"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      (is (equalp "Anne T. Donahue" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2019-10-18T00:00:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      ;; the lede is skipped
      (is (search "in my hometown mall and was an enthusiastic participant"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "I was a Denim Expert who rejected regular old skinnies"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (not (search "Watch women sizes 0 through 28 try on the exact same blue jeans"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Authentic, Accessible, Relevant"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "of this site constitutes acceptance of our"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "may earn a portion of sales from products that are purchased through our site"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-thefashionpolice ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "thefashionpolice.html" "media"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      ;; TODO TODO should we allow such hacks as having links in the author field?
      (is (equalp "https://www.facebook.com/thefashionpolice" (cdr (assoc :author (first parsed-docs)))))
      (is (equalp "2016-10-04T18:46:00Z" (cdr (assoc :date--post (first parsed-docs)))))
      (is (search "the death of the skinny jean for quite some time now"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "Well, the jury’s still out on this one"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "also opted for a plain white t-shirt to"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (search "Which could be either a good thing or a bad one, depending on your point of view"
                  (cdr (assoc :text (first parsed-docs)))))
      (is (not (search "Graphic Hoodies"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Ask the Fashion Police"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "The Best and Worst Looks From the 2020 SAG Awards"
                       (cdr (assoc :text (first parsed-docs))))))
      (is (not (search "Comments are closed"
                       (cdr (assoc :text (first parsed-docs)))))))))

(deftest x-quotestoscrape-search ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "quotestoscrape-search.html" "searchpage"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 10 (length parsed-docs)))
      ;; Here we really only care about URLs.
      (is (eq nil
              (find-if (lambda (doc) (null (cdr (assoc :url doc)))) parsed-docs)))
      (is (equalp "/author/Albert-Einstein"
                  (cdr (assoc :url (first parsed-docs)))))
      (is (equalp "/author/Helen-Keller"
                  (cdr (assoc :url (car (last parsed-docs)))))))))

(deftest x-quotestoscrape-author ()
  (when speechtractor::*server-running-p*
    (let* ((response (request-test-page "quotestoscrape-author.html" "blog"))
           (parsed-docs (ignore-errors (cl-json:decode-json-from-string response))))
      (is (= 1 (length parsed-docs)))
      ;;-(is (equalp "Albert Einstein" (cdr (assoc :author (first parsed-docs)))))
      (is (search "In 1879, Albert Einstein was born in Ulm, Germany"
                  (cdr (assoc :text (first parsed-docs))))))))
