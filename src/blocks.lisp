;;;; The block parser.
;;;;
;;;; This layer makes one pass over the line reader.
;;;;
;;;; Each step examines the current line, chooses the construct that starts
;;;; there, and calls the reader for that construct. The construct reader
;;;; consumes only that construct.
;;;;
;;;; The parser does not backtrack. A malformed document fails at the line that
;;;; caused the problem.

(in-package #:adoc)

(defparameter +listing-delimiter+ "----"
  "Delimiter line for listing blocks.")

(defparameter +stem-delimiter+ "++++"
  "Delimiter line for stem and passthrough blocks.")

(defun count-leading (character string)
  "Return the number of CHARACTER occurrences at the start of STRING."
  (or (position-if-not (lambda (each) (char= each character)) string)
      (length string)))

(defun heading-line-p (line)
  "Return the heading level of LINE.

Return NIL if LINE is not a heading.

A heading is one to four equals signs, one space, and a title."
  (let ((level (count-leading #\= line)))
    (when (and (<= 1 level 4)
               (< level (length line))
               (char= (char line level) #\Space)
               (plusp (length (trim-whitespace (subseq line level)))))
      level)))

(defun delimiter-line-p (line delimiter)
  "Return true when LINE contains only DELIMITER."
  (and line (string= (trim-whitespace line) delimiter)))

(defun listing-delimiter-p (line)
  "Return true when LINE opens or closes a listing block."
  (delimiter-line-p line +listing-delimiter+))

(defun stem-delimiter-p (line)
  "Return true when LINE opens or closes a stem or passthrough block."
  (delimiter-line-p line +stem-delimiter+))

(defun block-attribute-line (line)
  "Return T and the values when LINE is a block attribute line.

All block attributes use this parser. For example, `[source,lisp]`, `[stem]`,
and `[pass]` differ only in their values."
  (let ((trimmed (and line (trim-whitespace line))))
    (when (and trimmed
               (>= (length trimmed) 2)
               (char= (char trimmed 0) #\[)
               (char= (char trimmed (1- (length trimmed))) #\]))
      (values t (parse-attribute-list (subseq trimmed 1 (1- (length trimmed))))))))

(defun attribute-named-p (attributes name)
  "Return true when the first value in ATTRIBUTES is NAME.

Comparison ignores case."
  (and attributes (string-equal (first attributes) name)))

(defun item-content (line marker)
  "Return the text of LINE after MARKER.

Return NIL if LINE does not start with MARKER followed by a space.

The marker must be in column zero. The required space keeps `*bold* first` as
paragraph text, not a list item."
  (when (and line
             (>= (length line) 2)
             (char= (char line 0) marker)
             (char= (char line 1) #\Space))
    (trim-whitespace (subseq line 2))))

(defun unordered-item-p (line)
  "Return true when LINE starts an unordered list item."
  (and (item-content line #\*) t))

(defun ordered-item-p (line)
  "Return true when LINE starts an ordered list item."
  (and (item-content line #\.) t))

(defparameter +admonition-labels+
  '(("NOTE" . :note)
    ("TIP" . :tip)
    ("WARNING" . :warning)
    ("IMPORTANT" . :important)
    ("CAUTION" . :caution))
  "Admonition labels recognized by cl-adoc, with their node kinds.")

(defun parse-admonition-line (line)
  "Return the admonition kind and text of LINE.

Return NIL if LINE does not start an admonition.

The label must match exactly. A paragraph that starts with `NOTES: ` is prose."
  (when line
    (loop for (label . kind) in +admonition-labels+
          for length = (length label)
          when (and (> (length line) (1+ length))
                    (string= label line :end2 length)
                    (char= (char line length) #\:)
                    (char= (char line (1+ length)) #\Space))
            do (return (values kind (subseq line (+ length 2)))))))

(defun parse-quoted-line (line)
  "Return the text of a quoted LINE.

Return NIL when LINE is not quoted.

A bare `>` is an empty line inside the quote. The space after `>` is required,
so `>text` is prose."
  (when (and line (plusp (length line)) (char= (char line 0) #\>))
    (cond ((= (length line) 1) "")
          ((char= (char line 1) #\Space) (subseq line 2)))))

(defparameter +block-image-prefix+ "image::"
  "Prefix for the block image macro.")

(defun parse-block-image-line (line)
  "Return the target and raw attribute text of an `image::` line.

Return NIL if LINE is not a block image macro.

The closing bracket must be the last character on the line. This separates a
block macro from prose that mentions one."
  (let ((prefix-length (length +block-image-prefix+)))
    (when (and line
               (> (length line) prefix-length)
               (string= +block-image-prefix+ line :end2 prefix-length))
      (let ((open (position #\[ line :start prefix-length)))
        (when (and open (> open prefix-length))
          (let ((close (position #\] line :start (1+ open))))
            (when (and close (= close (1- (length line))))
              (values (subseq line prefix-length open)
                      (subseq line (1+ open) close)))))))))

(defparameter +description-delimiter+ "::"
  "Delimiter between a description term and its definition.")

(defun description-delimiter-position (line)
  "Return the position of the `::` that starts a description item.

Return NIL if LINE does not contain a description delimiter.

The delimiter is the first `::` that meets both conditions:

* it has a term before it
* it is followed by a space or by the end of the line

This rule keeps `image::path[alt]` as an image macro. In that macro, the target
starts immediately after `::`.

The rule also lets a term contain colons. For example, `:date:`:: is read as
the term `:date:`."
  (loop with limit = (length line)
        for position = (search +description-delimiter+ line)
          then (search +description-delimiter+ line :start2 (1+ position))
        while position
        for after = (+ position 2)
        when (and (plusp position)
                  (or (= after limit) (char= (char line after) #\Space)))
          return position))

(defun parse-description-line (line)
  "Return the term and definition text of LINE.

Return NIL if LINE does not start a description item."
  (when line
    (let ((position (description-delimiter-position line)))
      (when position
        (let ((term (trim-whitespace (subseq line 0 position))))
          (when (plusp (length term))
            (values term
                    (trim-whitespace (subseq line (min (length line)
                                                       (+ position 2)))))))))))

(defun block-boundary-p (line)
  "Return true when LINE cannot continue the current paragraph."
  (or (blank-line-p line)
      (heading-line-p line)
      (listing-delimiter-p line)
      (unordered-item-p line)
      (ordered-item-p line)
      (and (parse-admonition-line line) t)
      (and (parse-quoted-line line) t)
      (and (parse-block-image-line line) t)
      (and (parse-description-line line) t)
      (stem-delimiter-p line)
      (and (block-attribute-line line) t)))

;;; Individual constructs

(defun read-heading (reader)
  "Consume a heading from READER."
  (let* ((line-number (current-line-number reader))
         (line (next-line reader))
         (level (heading-line-p line)))
    (make-heading :level level
                  :line line-number
                  :content (parse-inline (trim-whitespace (subseq line level))))))

(defun read-delimited-body (reader delimiter)
  "Consume a block bounded by DELIMITER.

Return the block text and the opening line number."
  (let ((opened-on (current-line-number reader)))
    (next-line reader)                  ; consume the opening delimiter
    (let ((collected '()))
      (loop for line = (peek-line reader)
            until (or (null line) (delimiter-line-p line delimiter))
            do (push (next-line reader) collected)
            finally (when (null line)
                      (error 'unterminated-block
                             :line opened-on
                             :delimiter delimiter
                             :message (format nil "Block opened with ~a was never closed"
                                              delimiter))))
      (next-line reader)                ; consume the closing delimiter
      (values (format nil "~{~a~^~%~}" (nreverse collected)) opened-on))))

(defun read-listing (reader attributes)
  "Consume a listing block from READER.

Use ATTRIBUTES to set the language when the block has `[source,lang]`."
  (multiple-value-bind (text opened-on) (read-delimited-body reader +listing-delimiter+)
    (make-listing :line opened-on
                  :language (when (attribute-named-p attributes "source")
                              (second attributes))
                  :text text)))

(defun read-stem-or-passthrough (reader attributes)
  "Consume a `++++` block from READER.

The delimiter is used for two constructs.

`[pass]` makes the block passthrough text. The body reaches the output
unchanged.

All other cases make the block mathematics. This includes `[stem]` and a bare
delimiter.

Asciidoctor uses a different default. It treats a bare `++++` block as
passthrough. cl-adoc treats it as mathematics because math is the main reason
this project uses AsciiDoc."
  (multiple-value-bind (text opened-on) (read-delimited-body reader +stem-delimiter+)
    (if (attribute-named-p attributes "pass")
        (make-passthrough :line opened-on :text text)
        (make-block-stem :line opened-on :text text))))

(defun read-paragraph (reader)
  "Consume a paragraph from READER.

Stop before the first line that opens another block."
  (let ((line-number (current-line-number reader))
        (collected '()))
    (loop for line = (peek-line reader)
          until (or (null line) (block-boundary-p line))
          do (push (next-line reader) collected))
    (make-paragraph :line line-number
                    :content (parse-inline (format nil "~{~a~^~%~}" (nreverse collected))))))

(defun read-admonition (reader)
  "Consume an admonition from READER.

Include continuation lines that wrap onto the admonition paragraph."
  (let ((line-number (current-line-number reader)))
    (multiple-value-bind (kind text) (parse-admonition-line (next-line reader))
      (let ((collected (list text)))
        (loop for line = (peek-line reader)
              until (or (null line) (block-boundary-p line))
              do (push (next-line reader) collected))
        (make-admonition :line line-number
                         :kind kind
                         :content (parse-inline
                                   (format nil "~{~a~^~%~}" (nreverse collected))))))))

(defun quoted-paragraphs (numbered-lines)
  "Group quoted NUMBERED-LINES into paragraphs.

Each entry in NUMBERED-LINES is a cons of line number and text.

A blank line ends the current paragraph. This is how a bare `>` separates
paragraphs inside a quotation."
  (let ((paragraphs '())
        (current '()))
    (flet ((flush ()
             (when current
               (let ((lines (nreverse current)))
                 (push (make-paragraph
                        :line (car (first lines))
                        :content (parse-inline
                                  (format nil "~{~a~^~%~}" (mapcar #'cdr lines))))
                       paragraphs))
               (setf current '()))))
      (dolist (line numbered-lines)
        (if (blank-line-p (cdr line))
            (flush)
            (push line current)))
      (flush))
    (nreverse paragraphs)))

(defun read-blockquote (reader)
  "Consume a run of quoted lines from READER."
  (let ((line-number (current-line-number reader))
        (collected '()))
    (loop for text = (parse-quoted-line (peek-line reader))
          while text
          do (push (cons (current-line-number reader) text) collected)
             (next-line reader))
    (make-blockquote :line line-number
                     :content (quoted-paragraphs (nreverse collected)))))

(defun read-block-image (reader)
  "Consume an `image::` line from READER."
  (let ((line-number (current-line-number reader)))
    (multiple-value-bind (target attributes) (parse-block-image-line (next-line reader))
      (make-block-image :line line-number
                        :target target
                        :alt (first (parse-attribute-list attributes))))))

(defun parse-checkbox (text)
  "Return the checkbox state at the start of TEXT and the remaining text.

The state is :CHECKED, :UNCHECKED, or NIL."
  (when (and (>= (length text) 3)
             (char= (char text 0) #\[)
             (char= (char text 2) #\])
             ;; The bracket group must be its own word.
             ;; Thus, `[x]y` remains literal text.
             (or (= (length text) 3)
                 (char= (char text 3) #\Space)))
    (let ((rest (trim-whitespace (subseq text 3))))
      (case (char text 1)
        (#\Space (values :unchecked rest))
        ((#\x #\X) (values :checked rest))))))

(defun split-checkbox (text)
  "Split TEXT into checkbox state and content."
  ;; A backslash before `[` keeps the bracket literal.
  ;;
  ;; Checklist syntax is recognized only at the start of list item content.
  ;; This is the only place an author must escape a literal leading bracket.
  (if (and (> (length text) 1)
           (char= (char text 0) #\\)
           (char= (char text 1) #\[))
      (values nil (subseq text 1))
      (multiple-value-bind (state rest) (parse-checkbox text)
        (if state
            (values state rest)
            (values nil text)))))

(defun read-list-item (reader marker)
  "Consume one list item.

Include continuation lines that soft-wrap onto the item."
  (let* ((line-number (current-line-number reader))
         (collected (list (item-content (next-line reader) marker))))
    (loop for line = (peek-line reader)
          until (or (null line) (block-boundary-p line))
          ;; Continuation indentation is presentation, not content.
          do (push (trim-whitespace (next-line reader)) collected))
    (multiple-value-bind (checked content)
        (split-checkbox (format nil "~{~a~^~%~}" (nreverse collected)))
      (make-list-item :line line-number
                      :checked checked
                      :content (parse-inline content)))))

(defun read-list (reader ordered)
  "Consume a run of list items from READER as one list."
  (let ((line-number (current-line-number reader))
        (marker (if ordered #\. #\*))
        (items '()))
    (loop
      (push (read-list-item reader marker) items)
      ;; Blank lines can separate items in the same list.
      ;;
      ;; The main block loop would skip these lines too. Nothing must be
      ;; returned to the reader when the list ends.
      (skip-blank-lines reader)
      (unless (item-content (peek-line reader) marker)
        (return)))
    (let ((items (nreverse items)))
      (if ordered
          (make-ordered-list :line line-number :items items)
          (make-unordered-list :line line-number :items items)))))

(defun read-description-item (reader)
  "Consume one description item.

Include continuation lines that soft-wrap onto the definition."
  (let ((line-number (current-line-number reader)))
    (multiple-value-bind (term definition)
        (parse-description-line (next-line reader))
      (let ((collected (if (plusp (length definition)) (list definition) '())))
        ;; A definition continues like a list item.
        ;;
        ;; The term can have no definition on its line. In that case, the
        ;; definition starts on the following lines.
        (loop for line = (peek-line reader)
              until (or (null line) (block-boundary-p line))
              do (push (trim-whitespace (next-line reader)) collected))
        (make-description-item
         :line line-number
         :term (parse-inline term)
         :definition (parse-inline
                      (format nil "~{~a~^~%~}" (nreverse collected))))))))

(defun read-description-list (reader)
  "Consume a run of description items from READER as one list."
  (let ((line-number (current-line-number reader))
        (items '()))
    (loop
      (push (read-description-item reader) items)
      (skip-blank-lines reader)
      (unless (parse-description-line (peek-line reader))
        (return)))
    (make-description-list :line line-number :items (nreverse items))))

;;; Header and body

(defun attribute-line-p (line)
  "Return the name and value of a document attribute line.

Return NIL if LINE is not a `:key: value` line.

The name is the first value. The value is the second value."
  (when (and (plusp (length line)) (char= (char line 0) #\:))
    (let ((closing (position #\: line :start 1)))
      (when (and closing (> closing 1))
        (values (subseq line 1 closing)
                (trim-whitespace (subseq line (1+ closing))))))))

(defun read-header (reader)
  "Consume the document header from READER.

Return the title and the attributes.

The title is a list of inline nodes. The attributes are a hash table.

The header is the optional title line followed by attribute entries. It ends at
the first line that is neither a title nor an attribute."
  (let ((title nil)
        (attributes (make-hash-table :test #'equal)))
    (let ((line (peek-line reader)))
      (when (and line (eql (heading-line-p line) 1))
        (next-line reader)
        (setf title (parse-inline (trim-whitespace (subseq line 1))))))
    (loop for line = (peek-line reader)
          while line
          do (multiple-value-bind (name value) (attribute-line-p line)
               (unless name
                 (return))
               (setf (gethash name attributes) value)
               (next-line reader)))
    (values title attributes)))

(defun read-blocks (reader)
  "Consume all remaining blocks from READER."
  (let ((blocks '())
        (pending-attributes nil))
    (loop
      (skip-blank-lines reader)
      (let ((line (peek-line reader)))
        (when (null line)
          (return))
        (multiple-value-bind (attribute-line values) (block-attribute-line line)
          (if attribute-line
              ;; An attribute line configures the next block.
              ;; It is not a node.
              (progn (setf pending-attributes values)
                     (next-line reader))
              (progn
                (push (cond ((listing-delimiter-p line)
                             (read-listing reader pending-attributes))
                            ((stem-delimiter-p line)
                             (read-stem-or-passthrough reader pending-attributes))
                            ((heading-line-p line)
                             (read-heading reader))
                            ((parse-admonition-line line)
                             (read-admonition reader))
                            ((parse-quoted-line line)
                             (read-blockquote reader))
                            ((parse-block-image-line line)
                             (read-block-image reader))
                            ((unordered-item-p line)
                             (read-list reader nil))
                            ((ordered-item-p line)
                             (read-list reader t))
                            ;; Check description lists after block images.
                            ;;
                            ;; A block image target starts immediately after
                            ;; its colons, so it does not match a description
                            ;; item.
                            ((parse-description-line line)
                             (read-description-list reader))
                            (t
                             (read-paragraph reader)))
                      blocks)
                ;; An attribute applies only to the next block.
                (setf pending-attributes nil))))))
    (nreverse blocks)))
