
;;;; 
;;;; Reading explications from our filesystem "database".
;;;; 
(declaim (optimize (debug 3)))
(in-package :omnivore)

(defun explication-fun-symbol (name)
  (read-from-string (cl-strings:join (list "expl-" name))))

(defmacro atom-label-fun (atom-name atom-spec)
  `(,(explication-fun-symbol atom-name)
     () (create-graph :explication-definition
                      (first ,atom-spec)
                      (second ,atom-spec))))

;;;
;;; NOTE maybe these generate unevaluated code instead of being a macro (sbcl complains about uknown function expl- etc.)
;;;
(defmacro make-molecule-label-fun (molecule-name molecule-spec)
  `(,(explication-fun-symbol molecule-name)
     () (let* ((berry-graphs
                 (mapcar (lambda (berry-spec)
                           ;; Let the explication function create the graph for the berry.
                           (funcall (explication-fun-symbol (first berry-spec))))
                         (first ,molecule-spec)))
               (stalk-graphs
                 (mapcar (lambda (edge-spec)
                           (connection-graph :backward ;;; ???? TODO
                                             (or (third edge-spec)
                                                 ,#'graph-root-dangling-stalk)
                                             (nth (first edge-spec) berry-graphs)
                                             (nth (second edge-spec) berry-graphs)))
                         (second ,molecule-spec))))
          (apply #'concatenate-graphs (append berry-graphs stalk-graphs)))))

(defmacro collect-explications-and-run (lexeme-name)
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
                                    (get 'explication row-to-explicate))
                    (make-molecule-label-fun (get 'canonical-form row-to-explicate)
                                             (get 'explication row-to-explicate))))
          ;; Demand explications for referenced lexemes.
          (appendf lexeme-names
                   (mapcar #'first ; berry canonical form
                           (first (get 'explication row-to-explicate)))))))
    `(labels
       ,@(alexandria:hash-table-values name->label-fun) ; label function definitions
       ;; call our boy:
       (,(explication-fun-symbol lexeme-name)))))

(defun lexeme-known-p (word-canonical-form)
  (truep (directory (merge-pathnames  (pathname word-canonical-form)
                                      *filesystem-db-path*))))

;;; TODO handle valence!!!
;;; TODO handle meanings beyond 1!!!
(defun lexeme-lookup (word-canonical-form)
  "Return a lexeme row from the database."
  (let ((lexeme-dir-files (directory (merge-pathnames
                                       (pathname word-canonical-form)
                                       *filesystem-db-path*)))
        ;; Prefill with the canonical form.
        (lexeme-row (list 'canonical-form word-canonical-form)))
    ;; Throw an error if called for an unknown lexeme.
    (unless lexeme-dir-files (error (format nil "Unknown lexeme ~A" word-canonical-form)))
    ;; Indicate atomicity.
    (when (find "atom1" lexeme-dir-files :key #'file-namestring :test #'equalp)
      (setf (get 'atomp lexeme-row) t))
    ;; Get the explication definition.
    (let ((root-node-n (uiop:read-file-form ; this should also convert to an integer
                         (find "atom1" lexeme-dir-files
                               :key #'file-namestring :test #'equalp))))
      (setf (get 'explication lexeme-row)
            (graph-spec-from-xml (uiop:read-file-string
                                   (find "atom1" lexeme-dir-files
                                         :key #'file-namestring :test #'equalp))
                                 :root-node-n root-node-n)))
    lexeme-row))

;;; TODO check if it even exists in db.
(defun explication-lookup (word-canonical-form)
  "Return a graph."
  (when (lexeme-known-p word-canonical-form)
    (collect-explications-and-run word-canonical-form)))

;;;===(let ((atomic-lexemes (make-hash-table :test #'equalp)) ; maps of definitions
;;;===      (molecule-lexemes (make-hash-table :test #'equalp))
;;;===      (constituent-graphs)
;;;===      )
;;;===  (dolist (berry-spec (first graph-spec))
;;;===    ;; Add the graph definition for this unit to constituent-graphs.
;;;===    (if (and (equalp (first berry-spec) current-name)
;;;===             (gethash (first berry-spec) atomic-lexemes)
;;;===             ) 
;;;===        ;; Atoms are the only ones that just add themselves.
;;;===        (push (gethash (first berry-spec) atomic-lexemes)
;;;===               constituent-graphs
;;;===               )
;;;===        ;; Molecules add, ultimately, all their atoms and necessary connection graphs
;;;===        )
;;;===  ))
;;;===
