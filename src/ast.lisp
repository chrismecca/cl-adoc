;;;; The document tree.
;;;;
;;;; Nodes are structures, not CLOS instances.
;;;;
;;;; cl-adoc allocates many nodes and does not redefine them at runtime. The
;;;; structure representation is cheaper and does not remove needed behavior.
;;;; Structure classes can still specialize methods. The HTML renderer uses
;;;; that to dispatch on node type without a CASE over keywords.
;;;;
;;;; The tree is separate from rendering. It holds no output markup. Text is
;;;; stored as the author wrote it. A backend escapes text when it renders the
;;;; tree. This file has no HTML rules.

(in-package #:adoc)

;;; Block-level nodes

(defstruct (node (:constructor nil))
  "Base structure for block-level nodes."
  (line 0 :type (integer 0)))

(defstruct (heading (:include node))
  "A section heading.

LEVEL is the number of leading equals signs. A document title has level 1.
The deepest heading accepted by cl-adoc has level 4."
  (level 1 :type (integer 1 4))
  (content nil :type list))

(defstruct (paragraph (:include node))
  "A run of ordinary text.

CONTENT is a list of inline nodes."
  (content nil :type list))

(defstruct (listing (:include node))
  "A delimited listing block.

TEXT is literal. cl-adoc does not parse inline markup inside it."
  (language nil :type (or null string))
  (text "" :type string))

(defstruct (list-item (:include node))
  "One list item.

CHECKED is NIL for an ordinary item. It is :CHECKED or :UNCHECKED for a
checklist item."
  (content nil :type list)
  (checked nil :type (member nil :checked :unchecked)))

;; Ordered lists and unordered lists are separate types.
;;
;; They render to different elements. Separate types let the backend dispatch
;; on the node type instead of testing a flag.
;;
;; Description lists use the same pattern. They are a third list type, not a
;; flag on an existing list type.

(defstruct (description-list (:include node))
  "A list of terms and definitions.

ITEMS is a list of DESCRIPTION-ITEM nodes."
  (items nil :type list))

(defstruct (description-item (:include node))
  "One term and one definition.

TERM and DEFINITION are lists of inline nodes.

This structure does not reuse LIST-ITEM. A list item has one content slot. A
description item has two parts, and those parts render to different elements."
  (term nil :type list)
  (definition nil :type list))

(defstruct (block-stem (:include node))
  "A block of mathematics.

TEXT is the notation exactly as written. cl-adoc does not parse it. cl-adoc
passes it to the math renderer used by the page."
  (text "" :type string))

(defstruct (passthrough (:include node))
  "A block whose text reaches the output unchanged.

Escaping is not applied. This construct can emit arbitrary markup, so the
source document must request it by name."
  (text "" :type string))

(defstruct (admonition (:include node))
  "A NOTE, TIP, WARNING, IMPORTANT, or CAUTION.

CONTENT is one paragraph of inline nodes. This is the only admonition form
currently recognized."
  (kind :note :type (member :note :tip :warning :important :caution))
  (content nil :type list))

(defstruct (blockquote (:include node))
  "A run of lines prefixed with `> `.

CONTENT is a list of PARAGRAPH nodes. A bare `>` separates paragraphs inside
the quotation."
  (content nil :type list))

(defstruct (block-image (:include node))
  "An image that stands alone as a block.

ALT is NIL when the macro has no alt text. That is different from empty alt
text."
  (target "" :type string)
  (alt nil :type (or null string)))

(defstruct (unordered-list (:include node))
  "A bulleted list.

ITEMS is a list of LIST-ITEM nodes."
  (items nil :type list))

(defstruct (ordered-list (:include node))
  "A numbered list.

ITEMS is a list of LIST-ITEM nodes."
  (items nil :type list))

;;; Inline nodes
;;;
;;; Inline nodes do not carry line numbers. Each inline node is reached through
;;; its containing block, and that block records where it starts.

(defstruct (inline-node (:constructor nil))
  "Base structure for inline nodes.")

(defstruct (text (:include inline-node))
  "Literal text.

The string is unescaped and stored exactly as written."
  (string "" :type string))

(defstruct (strong (:include inline-node))
  "Bold text.

CONTENT is a list of inline nodes. This lets formatting nest."
  (content nil :type list))

(defstruct (emphasis (:include inline-node))
  "Italic text.

CONTENT is a list of inline nodes."
  (content nil :type list))

(defstruct (link (:include inline-node))
  "A hyperlink.

CONTENT is a list of inline nodes. Link text can contain formatting.

For a bare URL, CONTENT is the URL as literal text."
  (target "" :type string)
  (content nil :type list))

(defstruct (inline-stem (:include inline-node))
  "Mathematics inside a run of text.

The content is a flat string. cl-adoc does not parse it as markup."
  (text "" :type string))

(defstruct (inline-image (:include inline-node))
  "An image inside a line of text."
  (target "" :type string)
  (alt nil :type (or null string)))

(defstruct (monospace (:include inline-node))
  "A code span.

The content is a flat string, not a list of inline nodes. cl-adoc treats code
span content as literal text. For example, `foo_bar_` keeps its underscores
and does not contain emphasis."
  (string "" :type string))

;;; Documents

(defstruct document
  "A parsed AsciiDoc document.

TITLE is a list of inline nodes. It is NIL when the document has no header
title."
  (title nil :type list)
  (attributes (make-hash-table :test #'equal) :type hash-table)
  (blocks nil :type list))

(defun document-attribute (document name &optional default)
  "Return the header attribute NAME from DOCUMENT.

Return DEFAULT when the attribute is not set."
  (gethash name (document-attributes document) default))
