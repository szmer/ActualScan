(in-package :speechtractor-test)

(defparameter *test-document*
  "<html><head><title>Test document</title></head> \
                    <body><section data-author='isaac'><p>Hello there</p>\ 
                    <p>Green mooshrums? Mooks</p>\ 
                    <date>Jan 1, 2050</date></section> \
                    <section> <p>Farewell</p>\ 
                    <a href='www.example.com/somewhere' title='Permalink'>go there</a>\ 
                    <p>Really gigantic molluscs. I did it again!</p></section> \
                    </body></html>")

(defparameter *test-metadata-funs*
  (list :doc-startp (lambda (node path) (equalp (plump:tag-name node) "section"))
                 :author (lambda (node path)
                           (when (plump:has-attribute node "data-author")
                             (plump:attribute node "data-author")))
                 :date (lambda (node path) (when (equalp (plump:tag-name node) "date")
                                             (plump:render-text node)))
                 :permalink (lambda (node path)
                              (when (and (plump:has-attribute node "title")
                                         (plump:has-attribute node "href")
                                         (equalp "Permalink" (plump:attribute node "title")))
                                (plump:attribute node "href")))))

(deftest read-document ()
  (multiple-value-bind (paragraphs docs-metadata)
    (speechtractor::html-document-data
      *test-document* *test-metadata-funs*
      :classification-settings (alexandria:alist-hash-table
                                 (list (cons :length-low 3) (cons :length-high 10)
                                       (cons :stopwords-high 0) (cons :stopwords-low 0))))
    ;; Handling paragraphs and documents.
    (is (= 6 (length paragraphs)))
    (is (equalp "Hello there"
                (speechtractor::paragraph-text (first paragraphs) :cleanp t)))
    (is (equalp "Jan 1, 2050"
                (speechtractor::paragraph-text (third paragraphs) :cleanp t)))
    (is (eq (length docs-metadata) 2))
    ;; Classification.
    (is (eq :good (speechtractor::paragraph-classification (first paragraphs))))
    (is (eq :good (speechtractor::paragraph-classification (second paragraphs))))
    (is (eq :good (speechtractor::paragraph-classification (third paragraphs))))
    (is (eq :good (speechtractor::paragraph-classification (fourth paragraphs))))
    (is (eq :bad (speechtractor::paragraph-classification (fifth paragraphs)))) ; the link
    (is (eq :good (speechtractor::paragraph-classification (sixth paragraphs))))
    ;; Docstarts.
    (is (speechtractor::paragraph-doc-startp (first paragraphs)))
    (is (not (speechtractor::paragraph-doc-startp (second paragraphs))))
    (is (speechtractor::paragraph-doc-startp (fourth paragraphs)))
    ;; Metadata.
    (is (equalp "isaac" (getf (first docs-metadata) :author)))
    (is (equalp "Jan 1, 2050" (getf (first docs-metadata) :date)))
    (is (null (getf (first docs-metadata) :permalink)))
    (is (null (getf (second docs-metadata) :author)))
    (is (equalp "www.example.com/somewhere" (getf (second docs-metadata) :permalink)))))

(deftest read-document-json ()
  (let* ((json (speechtractor::html-document-data-json
                *test-document* *test-metadata-funs*
                :classification-settings (alexandria:alist-hash-table
                                           (list (cons :length-low 3) (cons :length-high 10)
                                                 (cons :stopwords-high 0) (cons :stopwords-low 0)))))
         (reread-json (cl-json:decode-json-from-string json)))
    (is (= 2 (length reread-json)))
    (is (equalp "Jan 1, 2050" (cdr (assoc :date (first reread-json)))))
    (is (equalp "isaac" (cdr (assoc :author (first reread-json)))))
    (is (equalp (format nil "Hello there~%~%Green mooshrums? Mooks~%~%Jan 1, 2050")
                (cdr (assoc :text (first reread-json)))))
    (is (equalp (format nil "Farewell~%~%Really gigantic molluscs. I did it again!")
                (cdr (assoc :text (second reread-json)))))
    (is (equalp "www.example.com/somewhere" (cdr (assoc :permalink (second reread-json)))))))


(deftest read-document-json-sents ()
  (let* ((json (speechtractor::html-document-data-json
                *test-document* *test-metadata-funs*
                :classification-settings (alexandria:alist-hash-table
                                           (list (cons :length-low 3) (cons :length-high 10)
                                                 (cons :stopwords-high 0) (cons :stopwords-low 0)))
                :split-sents t))
         (reread-json (cl-json:decode-json-from-string json)))
    (is (equalp (format nil "Hello there~%~%Green mooshrums?~%Mooks~%~%Jan 1, 2050")
                (cdr (assoc :text (first reread-json)))))
    (is (equalp (format nil "Farewell~%~%Really gigantic molluscs.~%I did it again!")
                (cdr (assoc :text (second reread-json)))))))
