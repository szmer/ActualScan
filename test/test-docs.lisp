(in-package :speechtractor-test)

(deftest read-document ()
  (let* ((document "<html><head><title>Test document</title></head> \
                    <body><section data-author='isaac'><p>Hello there</p>\ 
                    <p>Green mooshrums</p>\ 
                    <date>Jan 1, 2050</date></section> \
                    <section><p>Farewell</p>\ 
                    <a href='www.example.com/somewhere' title='Permalink'>go there</a>\ 
                    <p>Big molluscs</p></section> \
                    </body></html>")
         (metadata-funs
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
                                (plump:attribute node "href"))))))
        (multiple-value-bind (paragraphs docs-metadata)
          (speechtractor::html-document-data
            document metadata-funs
            :classification-settings (alexandria:alist-hash-table
                                       (list (cons :length-low 10) (cons :length-high 40))))
          (is (= 4 (length paragraphs)))
          (is (equalp "Hello there"
                      (speechtractor::paragraph-text (first paragraphs) :cleanp t)))
          (is (eq (length docs-metadata) 2))
          (is (equalp "isaac" (getf (first docs-metadata) :author)))
          (is (equalp "Jan 1, 2050" (getf (first docs-metadata) :date)))
          (is (equalp "www.example.com/somewhere" (getf (second docs-metadata) :permalink))))))
