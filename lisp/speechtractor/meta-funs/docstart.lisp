(in-package :speechtractor)

(defun full-html-doc-startp (node path)
  (equalp "html" (plump:tag-name node)))

(defun forums-doc-startp (node path)
  (or
    (and (plump:has-attribute node "class")
         (or
           ;; Xenforo
           (cl-ppcre:scan (boundary-regex "message") (plump:attribute node "class"))
           ;; some old ver of phpBB
           (cl-ppcre:scan (boundary-regex "thread_post") (plump:attribute node "class"))
           ;; Wallstreetoasis
           (cl-ppcre:scan (boundary-regex "comment") (plump:attribute node "class"))
           ))
    (and (plump:has-attribute node "typeof")
         ;; Wallstreetoasis first post
         (cl-ppcre:scan (boundary-regex "sioct:BoardPost") (plump:attribute node "typeof")))
    (and (equalp "div" (plump:tag-name node))
         (plump:has-attribute node "class")
         (or
           ;; some Wordpress forums
           (cl-ppcre:scan (boundary-regex "reply") (plump:attribute node "class"))
           ;; (these forums have the first post without characteristic DOM)
           (cl-ppcre:scan (boundary-regex "topic-author") (plump:attribute node "class"))))
    (and
      (plump:has-attribute node "class")
      ;; thestudentroom
      (cl-ppcre:scan (boundary-regex "post") (plump:attribute node "class"))
      (typep node 'plump:child-node)
      (plump:has-attribute (plump:parent node) "id")
      (equalp "postsContainer" (plump:attribute (plump:parent node) "id")))))

(defun searchpage-doc-startp (node path)
  (or
    ;; quotes.toscrape.com
   (and (plump:has-attribute node "itemtype")
        (equalp "http://schema.org/CreativeWork" (plump:attribute node "itemtype")))
   (and (plump:has-attribute node "class")
        (or
         ;; Test quotes.toscrape.com
         (cl-ppcre:scan (boundary-regex "next") (plump:attribute node "class"))
         ;; Wordpress search
         (cl-ppcre:scan (boundary-regex "post") (plump:attribute node "class"))
         ;; CNET, some thin Solr wrapping?
         (search "searchItem" (plump:attribute node "class"))
         (search "next" (plump:attribute node "class"))
         ;; Reuters.
         (cl-ppcre:scan (boundary-regex "search-result-indiv") (plump:attribute node "class"))
         ;; CNN: cnn-search__result--article or gallery
         (cl-ppcre:scan "result-+article" (plump:attribute node "class"))
         (cl-ppcre:scan "result-+gallery" (plump:attribute node "class"))
         ;; Where the search item may be preceded by date (e.g. New Scientist)
         ;;(cl-ppcre:scan (boundary-regex "published-date") (plump:attribute node "class"))
         (cl-ppcre:scan (boundary-regex "card") (plump:attribute node "class"))
         ;; Toms Hardware.
         (cl-ppcre:scan (boundary-regex "listingResult") (plump:attribute node "class"))
         ;; Gatesnotes.
         (cl-ppcre:scan "SearchThumb" (plump:attribute node "class"))
         ;; Pagination area - get the "next" link from the inside
         (cl-ppcre:scan (boundary-regex "search-pagination")
                        (plump:attribute node "class"))))))
