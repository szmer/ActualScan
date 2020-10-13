(in-package :omnivore2-test)

(deftest solr-query-field-from-period-alist ()
  (let ((test-period-alist '((:average--word--length . 175/44) (:period--number . 1)
                             (:parent--document--length . 3) (:sent--length . 2) (:word--length . 38)
                             (:text
                              . "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size.")
                             (:url . "http://example.com/hgwells/shorthistory/carbon200.html")
                             (:tags . "carbon paleozoic prehistory")
                             (:date--post . "1922-12-02T13:14:00Z")
                             (:date--retr . "1966-12-02T13:14:00Z"))))
    (is (equalp (omnivore2::solr-query-field-from-period-alist test-period-alist)
                "q=tags:carbon paleozoic prehistory"))))

(deftest solr-query-boost-from-period-alist ()
  (let* ((test-period-alist '((:average--word--length . 175/44) (:period--number . 1)
                              (:parent--document--length . 3) (:sent--length . 2) (:word--length . 38)
                              (:text
                               . "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size.")
                              (:url . "http://example.com/hgwells/shorthistory/carbon200.html")
                              (:tags . "carbon paleozoic prehistory")
                              (:date--post . "1922-12-02T13:14:00Z")
                              (:date--class . "1999-12-02T13:14:00Z")
                              (:date--retr . "1966-12-02T13:14:00Z")))
         (test-boost-field (omnivore2::solr-boost-field-from-period-alist test-period-alist)))
    (is (cl-strings:starts-with test-boost-field "bf="))
    (is (= (count #\+ test-boost-field)
           ;; text, url, tags, ignored dates, first terms doesn't need a plus
           (- (length test-period-alist) 3 2 1)))
    (is (search "average_word_length" test-boost-field))
    (is (search "date_post" test-boost-field))
    ;; Ensure that ratios are parenthesized.
    (is (search "(175/44)" test-boost-field))
    (is (not (search "NIL" test-boost-field)))
    ;; We don't want to order the context by date_retr or date_class
    (is (not (search "date_retr" test-boost-field)))
    (is (not (search "date_class" test-boost-field)))))
