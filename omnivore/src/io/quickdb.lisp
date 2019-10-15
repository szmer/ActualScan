
;;;;; 
;;;;; Reading explications from our filesystem "database".
;;;;; 
(declaim (optimize (debug 3)))
(in-package :omnivore)

;;;;
;;;; DB interface.
;;;;

(defun lexeme-known-p (word-canonical-form)
  (truep (directory (merge-pathnames  (pathname word-canonical-form)
                                      *filesystem-db-path*))))
;;; TODO handle valence!!!
;;;
;;; TODO handle meanings beyond 1!!!
;;;
;;; TODO Berries are always marked as of :explication-definition creator, even though they may be
;;; prototype. There is probably no way to pre-mark them in gexf (?)
(defun lexeme-lookup (word-canonical-form)
  "Return a lexeme row from the database."
  (let ((lexeme-dir-files (directory
                            ;; a wild pathname, indicating any file extension, is needed for dir
                            ;; listings.
                            (merge-pathnames (cl-strings:join
                                               (list
                                                 (pathname word-canonical-form)
                                                 "/*.*"))
                                             *filesystem-db-path*)))
        (lexeme-row nil))
    ;; Throw an error if called for an unknown lexeme.
    (unless lexeme-dir-files (error (format nil "Unknown lexeme '~A'" word-canonical-form)))
    ;; Enter the canonical form. (adding it in the let doesn't seem to work)
    (setf (get 'canonical-form lexeme-row) word-canonical-form)
    ;; Indicate atomicity.
    (if (find "atom1" lexeme-dir-files :key #'file-namestring :test #'equalp)
        (setf (get 'atomp lexeme-row) t)
        (setf (get 'atomp lexeme-row) nil))
    ;; Get the explication definition and root-node-n.
    (setf (get 'root-node-n lexeme-row)
          (uiop:read-file-form ; this should also convert to an integer
            (or (find "root1" lexeme-dir-files
                      :key #'file-namestring :test #'equalp)
                (error "no root file"))))
    (setf (get 'explication lexeme-row)
          (graph-spec-from-xml (uiop:read-file-string
                                 (or
                                   (find "meaning1.gexf" lexeme-dir-files
                                         :key #'file-namestring :test #'equalp) 
                                   (error "no meaning gexf file")))))
    lexeme-row))

;;;;
;;;; Explication assembly (to the internal representation).
;;;;

(defun explication-fun-symbol (name)
  (read-from-string (cl-strings:join (list "expl-" name))))

(defun atom-label-fun (atom-name atom-spec root-node-n)
  `(,(explication-fun-symbol atom-name)
     () (create-graph :explication-definition
                      (pulled-to-front ,root-node-n (first ',atom-spec))
                      (second ',atom-spec))))

(defun make-molecule-label-fun (molecule-name molecule-spec root-node-n)
  `(,(explication-fun-symbol molecule-name)
     () (let* ((berry-graphs
                 (pulled-to-front
                   ,root-node-n
                   ;; We need to create the whole list here as a symbol, because I found no way to
                   ;; call dynamically created lexical function symbols except for feeding it inside
                   ;; a symbolic parenthesis.
                   ,(cons 'list
                          (mapcar (lambda (berry-spec)
                                    ;; Let the explication function create the graph for the berry.
                                    `(,(explication-fun-symbol (first berry-spec))))
                                  (first molecule-spec)))))
               (stalk-graphs
                 (mapcar (lambda (edge-spec)
                           (when (or (>= (first edge-spec) (length berry-graphs))
                                     (>= (second edge-spec) (length berry-graphs)))
                            (error (format
                                     nil
                                     "One of indices ~A is out of bounds for berries in ~A graph"
                                     edge-spec ,molecule-name)))
                           (connection-graph :backward ; backward in dependency tree teminology,
                                                       ; forward in ours
                                             (gethash (or (third edge-spec) "root")
                                                      *exit-name->dangling-stalk-function*)
                                             (nth (first edge-spec) berry-graphs)
                                             (nth (second edge-spec) berry-graphs)))
                         ;; TODO it would be nice to check for duplicates, which may trip up stalk
                         ;; creation.
                         (second ',molecule-spec))))
          (apply #'concatenate-graphs (append berry-graphs stalk-graphs)))))

(defun assembled-explication (lexeme-name)
  (let* ((name->label-fun (make-hash-table :test #'equalp)))
    ;; There's an impulse to collect explications as we define the higher levels, but it makes for
    ;; nesting macros with side effects, which leads to stack limit breaking on compilation. So
    ;; we define the higher function first, and then demand rows for the lexemes in references.
    (do* ((lexeme-names (list lexeme-name))
          (name-to-explicate (pop lexeme-names) (pop lexeme-names)))
      ((null name-to-explicate))
      (unless (gethash name-to-explicate name->label-fun)
        (let ((row-to-explicate (lexeme-lookup name-to-explicate))) ; pull from the db
          ;; Create the graph-creating label function for the current row.
          (setf (gethash name-to-explicate name->label-fun)
                (if (get 'atomp row-to-explicate)
                    (atom-label-fun (get 'canonical-form row-to-explicate)
                                    (get 'explication row-to-explicate)
                                    (get 'root-node-n row-to-explicate))
                    (make-molecule-label-fun (get 'canonical-form row-to-explicate)
                                             (get 'explication row-to-explicate)
                                             (get 'root-node-n row-to-explicate))))
          ;; Demand explications for referenced lexemes.
          (appendf lexeme-names
                   (mapcar #'first ; berry canonical form
                           (first (get 'explication row-to-explicate)))))))
    (eval
      `(labels
       ,(alexandria:hash-table-values name->label-fun) ; label function definitions
       ;; call our boy:
       (,(explication-fun-symbol lexeme-name))))))

;;; TODO test with some "enshrined" explications.
(defun explication-lookup (word-canonical-form)
  "Return a graph."
  (when (lexeme-known-p word-canonical-form)
    (assembled-explication word-canonical-form)))
