;;;; The inline parser.
;;;;
;;;; Inline AsciiDoc needs a grammar.
;;;;
;;;; Formatting spans can nest. Delimiters can be escaped. A delimiter can
;;;; start a span only at a word boundary. This keeps `foo_bar_baz` as one word
;;;; instead of an emphasis span inside text.
;;;;
;;;; These rules cannot be implemented safely with string replacement.
;;;; Replacement code cannot easily say "only match when the previous character
;;;; is not a letter." It also scans text that earlier replacements already
;;;; changed.
;;;;
;;;; cl-adoc parses inline text into nodes first. Rendering happens later, and
;;;; only once. Rendered markup is never scanned again.

(in-package #:adoc)

;;; Boundary and character rules

(defrule word-constituent (alphanumericp character))

(defrule space-char (or #\Space #\Tab #\Newline))

;; A delimiter can open a span only at a word boundary.
;;
;; At position zero, there is no previous character. The lookbehind fails, so
;; `!` succeeds. Start of input is therefore a boundary without a special case.
(defrule left-boundary (! (< 1 word-constituent)))

;;; Escapes
;;;
;;; A backslash makes the next delimiter literal.

(defrule escaped
    (and #\\ (or #\* #\_ #\` #\\))
  (:function second))

;;; Formatting spans
;;;
;;; Each formatted span has this form:
;;;
;;; * a word boundary
;;; * an opening delimiter
;;; * a non-space first character
;;; * a body
;;; * a non-space last character
;;; * a closing delimiter
;;;
;;; The space checks keep `2 * 3 * 4` as text. The trailing check is a
;;; lookbehind because the parser has already consumed the body.

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

;; Monospace is the only span whose body is not parsed.
;;
;; Its content is literal, so a code span keeps all punctuation inside it.
(defrule monospace
    (and left-boundary #\` (! space-char) monospace-body (< 1 (! space-char)) #\`)
  (:destructure (boundary open no-leading-space body no-trailing-space close)
    (declare (ignore boundary open no-leading-space no-trailing-space close))
    (make-monospace :string body)))

(defrule monospace-body
    (+ (and (! #\`) character))
  (:lambda (items) (coerce (mapcar #'second items) 'string)))

;;; Macros
;;;
;;; Each macro ends with a bracketed attribute list.
;;;
;;; Block image macros use the same attribute-list syntax. The block parser
;;; captures the raw bracket text and calls the same splitter used here.
;;;
;;; v1 has no escape for a closing bracket. An attribute list ends at the first
;;; `]`. A link text value that needs `]` is not supported yet.

(defrule bracketed-text
    (and #\[ (* (and (! #\]) character)) #\])
  (:destructure (open characters close)
    (declare (ignore open close))
    (coerce (mapcar #'second characters) 'string)))

(defun parse-attribute-list (text)
  "Split TEXT into positional attribute values.

Commas separate values.

Return NIL for an empty list. Thus, `image::photo.png[]` has no alt text. It
does not have empty alt text."
  (let ((values '())
        (start 0))
    (dotimes (index (length text))
      (when (char= (char text index) #\,)
        (push (trim-whitespace (subseq text start index)) values)
        (setf start (1+ index))))
    (push (trim-whitespace (subseq text start)) values)
    (let ((values (nreverse values)))
      (if (equal values '("")) '() values))))

(defrule macro-target
    (+ (and (! (or #\[ space-char)) character))
  (:lambda (characters) (coerce (mapcar #'second characters) 'string)))

(defrule link
    (and left-boundary "link:" macro-target bracketed-text)
  (:destructure (boundary prefix target text)
    (declare (ignore boundary prefix))
    (let ((values (parse-attribute-list text)))
      (make-link :target target
                 ;; `link:url[]` displays the URL.
                 :content (if values
                              (parse-inline (first values))
                              (list (make-text :string target)))))))

;; The negative lookahead keeps `image::` as a block image macro.
;;
;; Without it, `image::target[alt]` could become an inline image whose target
;; starts with a colon.
(defrule inline-image
    (and left-boundary "image:" (! #\:) macro-target bracketed-text)
  (:destructure (boundary prefix not-block target text)
    (declare (ignore boundary prefix not-block))
    (make-inline-image :target target
                       :alt (first (parse-attribute-list text)))))

;; Mathematics is literal for the same reason code spans are literal.
;;
;; Math notation uses characters that also have meaning in the inline grammar.
(defrule inline-stem
    (and left-boundary "stem:" bracketed-text)
  (:destructure (boundary prefix text)
    (declare (ignore boundary prefix))
    (make-inline-stem :text text)))

;;; Autolinks
;;;
;;; A bare URL must stop before trailing sentence punctuation.
;;;
;;; For example, in `See https://x.com/a.`, the period ends the sentence. It is
;;; not part of the URL.
;;;
;;; The parser must leave that period outside the link before it consumes the
;;; URL. Trimming it later would lose the punctuation as separate text.
;;;
;;; Trailing punctuation is part of the URL only when another URL character
;;; follows it. This keeps the period in `x.com`, but not the period after
;;; `x.com/a.`.

(defrule url-scheme (or "https://" "http://"))

(defrule url-punctuation (or #\. #\, #\; #\: #\! #\? #\) #\] #\}))

(defrule url-core
    (and (! (or space-char url-punctuation)) character)
  (:function second))

(defrule url-character
    (or url-core (and url-punctuation (& url-core)))
  (:lambda (value)
    (if (characterp value) value (char (first value) 0))))

(defrule autolink
    (and left-boundary url-scheme (+ url-character))
  (:destructure (boundary scheme rest)
    (declare (ignore boundary))
    (let ((url (concatenate 'string scheme (coerce rest 'string))))
      (make-link :target url :content (list (make-text :string url))))))

;;; Composition
;;;
;;; Ordered choice resolves conflicts.
;;;
;;; An escape is read before a delimiter can open a span. Monospace is read
;;; before other spans, so backticks protect their content. A single character
;;; is the fallback.
;;;
;;; An unmatched delimiter therefore stays literal. It does not fail the parse.

(defrule inline-element
    (or escaped monospace link inline-image inline-stem autolink
        strong emphasis character))

(defrule inline-content
    (* inline-element)
  (:lambda (elements) (coalesce-text elements)))

(defun coalesce-text (elements)
  "Merge runs of raw characters in ELEMENTS into TEXT nodes.

The grammar returns parsed nodes and unmatched characters. Any character that
does not start a construct becomes literal text.

This function combines those characters so the result is a list of inline
nodes."
  (let ((nodes '())
        (buffer (make-string-output-stream)))
    (flet ((flush ()
             (let ((pending (get-output-stream-string buffer)))
               (when (plusp (length pending))
                 (push (make-text :string pending) nodes)))))
      (dolist (element elements)
        (etypecase element
          ;; ESRAP returns a string for a character terminal and a character
          ;; for the CHARACTER rule. Both are text here.
          (character (write-char element buffer))
          (string (write-string element buffer))
          (inline-node (flush) (push element nodes))))
      (flush))
    (nreverse nodes)))

(defun parse-inline (string)
  "Parse STRING into inline nodes."
  (esrap:parse 'inline-content string))
