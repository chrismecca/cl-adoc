;;;; package.lisp

(defpackage #:adoc
  (:nicknames #:cl-adoc)
  (:use #:cl)
  ;; esrap's own operators have to be esrap's symbols: an unrecognised head
  ;; falls through to its semantic-predicate form, which then tries to call
  ;; the symbol as a function. The operators that collide with COMMON-LISP --
  ;; *, + and < -- are matched by name instead, so those read as themselves
  ;; inside a rule and are deliberately absent here.
  (:import-from #:esrap #:defrule #:! #:&)
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
   #:description-list
   #:description-list-p
   #:description-list-items
   #:description-item
   #:description-item-p
   #:description-item-term
   #:description-item-definition
   #:block-image
   #:block-image-p
   #:block-image-target
   #:block-image-alt
   #:admonition
   #:admonition-p
   #:admonition-kind
   #:admonition-content
   #:blockquote
   #:blockquote-p
   #:blockquote-content
   #:block-stem
   #:block-stem-p
   #:block-stem-text
   #:passthrough
   #:passthrough-p
   #:passthrough-text

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
   #:link
   #:link-p
   #:link-target
   #:link-content
   #:inline-image
   #:inline-image-p
   #:inline-image-target
   #:inline-image-alt
   #:inline-stem
   #:inline-stem-p
   #:inline-stem-text

   ;; Reading a parsed document
   #:node-children
   #:map-nodes
   #:collect-nodes
   #:find-node
   #:node-text
   #:heading-id
   #:document-headings
   #:document-uses-stem-p
   #:document-references
   #:reference-target

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
