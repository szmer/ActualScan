(in-package :omnivore2-test)

(deftest period-entries-from-alist ()
  (let* ((omnivore2::*good-minimal-period-length* 30)
         (omnivore2::*good-maximal-period-length* 50)
         (omnivore2::*common-ngrams-count-for-merging* 7)
         (test-text-alist (list (cons :text "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate.")
                                (cons :url "http://example.com/hgwells/shorthistory/carbon200.html")
                                (cons :date--post "1922-12-02T13:14:00Z")))
         (test-period-alists (omnivore2::period-entries-from-alist test-text-alist)))
    (is (= 3 (length test-period-alists)))
    (is (assoc :text (first test-period-alists)))
    ;; Make sure that period text doesn't simply link to the original.
    (is (not (equalp (cdr (assoc :text (first test-period-alists)))
                     (cdr (assoc :text test-text-alist)))))
    ;; Preserve the original features.
    (is (assoc :url (first test-period-alists)))
    (is (assoc :date--post (first test-period-alists)))
    ;; See if the additional features are present.
    (is (assoc :word--length (first test-period-alists)))
    (is (assoc :sent--length (first test-period-alists)))
    (is (assoc :parent--document--length (first test-period-alists)))
    (is (assoc :period--number (first test-period-alists)))
    ;; We want one-based period numbers.
    (is (= 1
           (cdr (assoc :period--number (first test-period-alists)))))))

(deftest test-stationary-analysis ()
  (let* ((omnivore2::*good-minimal-period-length* 30)
         (omnivore2::*good-maximal-period-length* 50)
         (omnivore2::*common-ngrams-count-for-merging* 7)
         (omnivore2::*stationary-analytic-funs* '(#'omnivore2::add-average-word-length-for-period))
         (test-text-alist (list (cons :text "All the air-breathing vertebrata of this age of swamps and plants belonged to the class amphibia. They were nearly all of them forms related to the newts of to-day, and some of them attained a considerable size. They were land animals, it is true, but they were land animals needing to live in and near moist and swampy places, and all the great trees of this period were equally amphibious in their habits. None of them had yet developed fruits and seeds of a kind that could fall on land and develop with the help only of such moisture as dew and rain could bring. They all had to shed their spores in water, it would seem, if they were to germinate.")
                          (cons :url "http://example.com/hgwells/shorthistory/carbon200.html")
                          (cons :date--post "1922-12-02T13:14:00Z")))
         (test-period-alists (omnivore2::stationary-analysis-applied test-text-alist)))
    (is (assoc :text (first test-period-alists)))
    (is (assoc :date--post (first test-period-alists)))
    (is (assoc :word--length (first test-period-alists)))
    (is (assoc :average--word--length (first test-period-alists)))
    ;; This ensures that JSON keys are encoded as they should (producing the underscores from double hyphens)
    (is (search "period_number" (cl-json:encode-json-to-string test-period-alists)))))
