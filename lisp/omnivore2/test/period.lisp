(in-package :omnivore2-test)

(deftest text-char-ngrams ()
  (is (equalp (omnivore2::text-char-ngrams 3 "The bear could bear it no more.")
              '(" be" " be" " co" " it" " mo" " no" "The" "ar " "ar " "bea" "bea" "cou" "d b"
                "e b" "ear" "ear" "he " "it " "ld " "mor" "no " "o m" "ore" "oul" "r c" "r i"
                "t n" "uld"))))

(deftest common-string-count ()
  (is (= 4
         (omnivore2::common-string-count
          (omnivore2::text-char-ngrams 3 "The bear could bear it no more.")
          (omnivore2::text-char-ngrams 3 "No jumping near me, the badger said."))))
  ;; The result is the same if we change the argument order.
  (is (= (omnivore2::common-string-count
          (omnivore2::text-char-ngrams 3 "No jumping near me, the badger said.")
          (omnivore2::text-char-ngrams 3 "The bear could bear it no more."))
         (omnivore2::common-string-count
          (omnivore2::text-char-ngrams 3 "The bear could bear it no more.")
          (omnivore2::text-char-ngrams 3 "No jumping near me, the badger said.")))))

(deftest periods-from-sentences ()
  (let* ((omnivore2::*good-minimal-period-length* 30)
         (omnivore2::*good-maximal-period-length* 50)
         (omnivore2::*common-ngrams-count-for-merging* 7)
         (test-sents (omnivore2::text-sentences "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate."))
         (test-periods (omnivore2::periods-from-sentences test-sents)))
    (is (= 3 (length test-periods)))
    (is (> (length (first test-periods)) 150))
    (is (< (length (first test-periods)) 250))
    (is (> (length (second test-periods)) 150))
    (is (< (length (second test-periods)) 250))
    (is (> (length (third test-periods)) 150))
    (is (< (length (third test-periods)) 250))))
