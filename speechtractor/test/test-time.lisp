(in-package :speechtractor-test)

(deftest dates ()
  (is (equalp "2019-09-20T06:35:00Z" (speechtractor::maybe-solr-date-from-str "Sep 20th, 2019 6:35 am")))
  (is (equalp "2018-10-15T00:00:00Z" (speechtractor::maybe-solr-date-from-str "Oct 15, 2018"))))
