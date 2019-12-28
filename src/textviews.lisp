(defpackage :textviews (:use :cl)
  (:export
    ;; Classes and accessors.
    :source :source-identifier :source-format :source-creator :source-title :source-publisher
    :source-publication-place :source-publication-date :source-editor :source-institution
    :source-uri :source-doi :source-meta-schemes :source-meta :source-template-meta
    :processing-layer :processing-identifier :processing-mechanism :processing-parameters
    :correction :correction-attr-name :correction-replaced-value :correction-processing-layer
    :correction-time
    :text-record :record-identifier :record-kind :record-parent :record-language :record-creator
    :record-publication-date :record-meta-schemes :record-meta :record-corrections
    :record-deferrables
    :text-object :object-raw-text :object-divisions
    :category :category-division-kind :category-divisions-range :category-attribute-name
    :category-attribute-value :category-criterion
    :corpus :corpus-version :corpus-contact :corpus-maintainer :corpus-sources
    :corpus-processing-layers :corpus-omited-attributes
    :view :view-corpus :view-method :view-category :view-subviews :view-values
    ;; Make functions.
    :make-source :make-correction :make-processing-layer :make-corpus :make-view
    ;; Types, other functions.
    :record-kind :read-attribute :raw-text :list-as-hash-set))

(in-package :textviews)

(defun list-as-hash-set (list)
  (alexandria:alist-hash-table
    (mapcar (lambda (elem) (cons elem t))
            list)
    :test #'equalp))

;;; TODO A function for accessing a slot or empty string?
