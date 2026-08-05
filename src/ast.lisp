;;;; The document tree.
;;;;
;;;; Nodes are structures rather than CLOS instances. They are allocated in
;;;; bulk and never redefined at runtime, so the cheaper representation costs
;;;; nothing in flexibility, and structure classes are still valid method
;;;; specializers -- which is what lets the renderer in html.lisp dispatch on
;;;; node type without a CASE over keywords.
;;;;
;;;; The split that matters here is between the tree and its rendering. The
;;;; tree holds no markup: text is stored exactly as the author typed it, and
;;;; escaping happens once, in a backend, on the way out. Nothing in this file
;;;; knows what HTML is.

(in-package #:adoc)

;;; Block-level nodes

(defstruct (node (:constructor nil))
  "Common base for block-level nodes."
  (line 0 :type (integer 0)))

(defstruct (heading (:include node))
  "A section heading. LEVEL is the number of leading equals signs, so a
document title is 1 and the deepest heading cl-adoc v1 accepts is 4."
  (level 1 :type (integer 1 4))
  (content nil :type list))

(defstruct (paragraph (:include node))
  "A run of ordinary text. CONTENT is a list of inline nodes."
  (content nil :type list))

(defstruct (listing (:include node))
  "A delimited listing block. TEXT is verbatim: no inline parsing is ever
applied to it, which is the whole point of the construct."
  (language nil :type (or null string))
  (text "" :type string))

(defstruct (list-item (:include node))
  "One entry in a list. CHECKED is NIL for an ordinary item, or :CHECKED or
:UNCHECKED when the item carries a checkbox."
  (content nil :type list)
  (checked nil :type (member nil :checked :unchecked)))

;; Ordered and unordered lists are separate types rather than one type with a
;; flag. They render to different elements, and keeping them apart means the
;; backend dispatches instead of branching -- which is also what leaves room
;; for description lists to arrive later as a third type.

(defstruct (unordered-list (:include node))
  "A bulleted list. ITEMS is a list of LIST-ITEM."
  (items nil :type list))

(defstruct (ordered-list (:include node))
  "A numbered list. ITEMS is a list of LIST-ITEM."
  (items nil :type list))

;;; Inline nodes
;;;
;;; These carry no line number. They are always reached through the block that
;;; contains them, and that block already knows where it started.

(defstruct (inline-node (:constructor nil))
  "Common base for inline nodes.")

(defstruct (text (:include inline-node))
  "Literal text, unescaped and exactly as written."
  (string "" :type string))

(defstruct (strong (:include inline-node))
  "Bold text. CONTENT is a list of inline nodes, so formatting nests."
  (content nil :type list))

(defstruct (emphasis (:include inline-node))
  "Italic text. CONTENT is a list of inline nodes."
  (content nil :type list))

(defstruct (monospace (:include inline-node))
  "A code span. Its contents are a flat string rather than a node list: cl-adoc
treats the inside of a code span as literal, so `foo_bar_` is three underscored
words and not an emphasis."
  (string "" :type string))

;;; Documents

(defstruct document
  "A parsed AsciiDoc document. TITLE is a list of inline nodes, or NIL when the
document has no header title."
  (title nil :type list)
  (attributes (make-hash-table :test #'equal) :type hash-table)
  (blocks nil :type list))

(defun document-attribute (document name &optional default)
  "Return the header attribute NAME from DOCUMENT, or DEFAULT if it is unset."
  (gethash name (document-attributes document) default))
