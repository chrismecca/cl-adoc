;;;; package.lisp

(defpackage #:adoc
  (:nicknames #:cl-adoc)
  (:use #:cl)
  ;; Only the operators the grammar actually names are imported. esrap
  ;; resolves the rest of its expression syntax by symbol name, so the
  ;; repetition operators read as plain * and + inside a rule even though
  ;; those symbols belong to COMMON-LISP here.
  (:import-from #:esrap #:defrule #:!)
  (:export
   ;; Entry points
   #:parse-string
   #:parse-file
   #:render-html

   ;; Document
   #:document
   #:document-p
   #:document-title
   #:document-attributes
   #:document-attribute
   #:document-blocks

   ;; Block nodes
   #:node
   #:node-line
   #:heading
   #:heading-p
   #:heading-level
   #:heading-content
   #:paragraph
   #:paragraph-p
   #:paragraph-content
   #:listing
   #:listing-p
   #:listing-language
   #:listing-text
   #:unordered-list
   #:unordered-list-p
   #:unordered-list-items
   #:ordered-list
   #:ordered-list-p
   #:ordered-list-items
   #:list-item
   #:list-item-p
   #:list-item-content
   #:list-item-checked

   ;; Inline nodes
   #:inline-node
   #:text
   #:text-p
   #:text-string
   #:strong
   #:strong-p
   #:strong-content
   #:emphasis
   #:emphasis-p
   #:emphasis-content
   #:monospace
   #:monospace-p
   #:monospace-string

   ;; Rendering
   #:backend
   #:html5
   #:render-node

   ;; Conditions
   #:adoc-error
   #:syntax-error
   #:syntax-error-line
   #:syntax-error-message
   #:unterminated-block
   #:unterminated-block-delimiter))
