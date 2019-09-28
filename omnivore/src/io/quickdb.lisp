
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

(defmacro make-molecule-label-fun (molecule-name molecule-spec explication-funs-table)
  `(,(explication-fun-symbol molecule-name)
     () (let* ((berry-graphs
                 (mapcar (lambda (berry-spec)
                           ;; Pull and record the missing lexeme labeled function if needed.
                           (unless (gethash (explication-fun-symbol (first berry-spec))
                                             ,explication-funs-table)
                              ;; (TODO This need not constantly call the database if we preload all
                              ;; explications from some a priori known list.)
                              (let ((lexeme-row (lexeme-lookup (first berry-spec))))
                                (setf 
                                  (gethash (explication-fun-symbol (first berry-spec))
                                           ,explication-funs-table)
                                  nil
                                  (if (get 'atomp lexeme-row)
                                      (atom-label-fun (first berry-spec)
                                                      (get 'explication lexeme-row))
                                      (make-molecule-label-fun (first berry-spec)
                                                               (get 'explication lexeme-row)
                                                               ,explication-funs-table)))))
                           ;; We will need just to let the function create the graph for the berry.
                           (funcall (explication-fun-symbol (first berry-spec))))
                         (first ,molecule-spec)))
               (stalk-graphs
                 (mapcar (lambda (edge-spec)
                           (connection-graph :backward ;;; ???? TODO
                                             (or (third edge-spec)
                                                 ,#'graph-root-dangling-stalk)
                                             (nth (first edge-spec) berry-graphs)
                                             (nth (second edge-spec) berry-graphs)
                                             )))))
          (apply #'concatenate-graphs (append berry-graphs stalk-graphs)))))

(defmacro collect-explications-and-run (lexeme-row)
  (let* ((label-funs-table (make-hash-table))) ; here leave explications indexed by their symbols
    (setf (gethash (explication-fun-symbol (get 'canonical-form lexeme-row))
                   label-funs-table)
          (make-molecule-label-fun (get 'canonical-form lexeme-row)
                                   (get 'explication lexeme-row)
                                   label-funs-table))
    `(labels
       ,@(alexandria:hash-table-values label-funs-table)
       (,(explication-fun-symbol (get 'canonical-form lexeme-row))))))

(defun lexeme-lookup (word-canonical-form)
  "Return a lexeme row from the database."
  (declare (ignore word-canonical-form))
  nil ; TODO
  )

(defun explication-lookup (word-canonical-form)
  "Return a graph."
  (collect-explications-and-run (lexeme-lookup word-canonical-form)))

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
