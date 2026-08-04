;;;; The inline layer.
;;;;
;;;; This is the part of AsciiDoc that genuinely needs a grammar. Formatting
;;;; spans nest, they can be escaped, and above all they are *constrained*: a
;;;; delimiter only opens a span at a word boundary. That last rule is why
;;;; foo_bar_baz is one word and not a fragment wrapped in emphasis.
;;;;
;;;; A constrained span cannot be expressed as a string substitution. "Replace
;;;; _x_ with <em>x</em>" has no way to say "only when the preceding character
;;;; was not a letter", and once it has rewritten part of the string, the next
;;;; substitution runs over markup the previous one emitted -- which is how an
;;;; italic pass ends up eating the underscores inside an already-rendered code
;;;; span. Parsing to nodes and rendering once removes the entire failure mode:
;;;; by the time anything is rendered there is no text left to re-scan.

(in-package #:adoc)

;;; Boundary and character rules

(defrule word-constituent (alphanumericp character))

(defrule space-char (or #\Space #\Tab #\Newline))

;; A delimiter may only open a span at a word boundary. At position zero there
;; is nothing behind the cursor, so the lookbehind fails and ! succeeds --
;; start of input counts as a boundary without needing a special case.
(defrule left-boundary (! (< 1 word-constituent)))

;;; Escapes
;;;
;;; A backslash suppresses the character that follows, which is the only way to
;;; write a literal delimiter next to a word.

(defrule escaped
    (and #\\ (or #\* #\_ #\` #\\))
  (:function second))

;;; Formatting spans
;;;
;;; Each span is: a word boundary, the delimiter, a non-space first character,
;;; a body, a non-space last character, the delimiter. The two space checks are
;;; what stop "2 * 3 * 4" from turning half a sentence bold. The trailing one
;;; is a lookbehind because at that point the body has already been consumed.

(defrule strong
    (and left-boundary #\* (! space-char) strong-body (< 1 (! space-char)) #\*)
  (:destructure (boundary open no-leading-space body no-trailing-space close)
    (declare (ignore boundary open no-leading-space no-trailing-space close))
    (make-strong :content body)))

(defrule strong-body
    (+ (and (! #\*) inline-element))
  (:lambda (items) (coalesce-text (mapcar #'second items))))

(defrule emphasis
    (and left-boundary #\_ (! space-char) emphasis-body (< 1 (! space-char)) #\_)
  (:destructure (boundary open no-leading-space body no-trailing-space close)
    (declare (ignore boundary open no-leading-space no-trailing-space close))
    (make-emphasis :content body)))

(defrule emphasis-body
    (+ (and (! #\_) inline-element))
  (:lambda (items) (coalesce-text (mapcar #'second items))))

;; Monospace is the one span whose body is not parsed. Its contents are taken
;; verbatim, so a code span survives whatever punctuation it happens to hold.
(defrule monospace
    (and left-boundary #\` (! space-char) monospace-body (< 1 (! space-char)) #\`)
  (:destructure (boundary open no-leading-space body no-trailing-space close)
    (declare (ignore boundary open no-leading-space no-trailing-space close))
    (make-monospace :string body)))

(defrule monospace-body
    (+ (and (! #\`) character))
  (:lambda (items) (coerce (mapcar #'second items) 'string)))

;;; Composition
;;;
;;; Ordered choice does the disambiguation. An escape is recognised before any
;;; delimiter can open, monospace outranks the other spans so that backticks
;;; win over their contents, and a bare character is the fallback -- which is
;;; how an unmatched delimiter degrades to the literal character the author
;;; typed rather than failing the parse.

(defrule inline-element
    (or escaped monospace strong emphasis character))

(defrule inline-content
    (* inline-element)
  (:lambda (elements) (coalesce-text elements)))

(defun coalesce-text (elements)
  "Merge runs of raw characters in ELEMENTS into TEXT nodes.

The grammar yields a mix of parsed nodes and unclaimed characters, since any
character that starts no construct falls through to the literal branch. This
gathers those stragglers so the result is a clean list of inline nodes."
  (let ((nodes '())
        (buffer (make-string-output-stream)))
    (flet ((flush ()
             (let ((pending (get-output-stream-string buffer)))
               (when (plusp (length pending))
                 (push (make-text :string pending) nodes)))))
      (dolist (element elements)
        (etypecase element
          ;; esrap yields a string for a character terminal and a character for
          ;; the CHARACTER rule; both are just text at this point.
          (character (write-char element buffer))
          (string (write-string element buffer))
          (inline-node (flush) (push element nodes))))
      (flush))
    (nreverse nodes)))

(defun parse-inline (string)
  "Parse STRING into a list of inline nodes."
  (esrap:parse 'inline-content string))

(defun inline-text (nodes)
  "Return the plain text of NODES with all formatting removed.
Used for deriving heading identifiers, where markup has no meaning."
  (with-output-to-string (out)
    (labels ((walk (node)
               (etypecase node
                 (text (write-string (text-string node) out))
                 (monospace (write-string (monospace-string node) out))
                 (strong (mapc #'walk (strong-content node)))
                 (emphasis (mapc #'walk (emphasis-content node))))))
      (mapc #'walk nodes))))
