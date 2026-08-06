;;;; The block layer.
;;;;
;;;; One pass over the line reader. Each iteration looks at the current line,
;;;; decides which construct it opens, and hands the reader to a function that
;;;; consumes exactly that construct. Nothing backtracks, so a malformed
;;;; document fails at the line that caused it rather than somewhere later.

(in-package #:adoc)

(defparameter +listing-delimiter+ "----"
  "Delimiter line opening and closing a listing block.")

(defparameter +stem-delimiter+ "++++"
  "Delimiter line opening and closing a stem or passthrough block.")

(defun count-leading (character string)
  "Return the length of the run of CHARACTER at the start of STRING."
  (or (position-if-not (lambda (each) (char= each character)) string)
      (length string)))

(defun heading-line-p (line)
  "Return the heading level of LINE, or NIL if it is not a heading.
A heading is one to four equals signs, a space, and a title."
  (let ((level (count-leading #\= line)))
    (when (and (<= 1 level 4)
               (< level (length line))
               (char= (char line level) #\Space)
               (plusp (length (trim-whitespace (subseq line level)))))
      level)))

(defun delimiter-line-p (line delimiter)
  "True when LINE consists of exactly DELIMITER."
  (and line (string= (trim-whitespace line) delimiter)))

(defun listing-delimiter-p (line)
  "True when LINE opens or closes a listing block."
  (delimiter-line-p line +listing-delimiter+))

(defun stem-delimiter-p (line)
  "True when LINE opens or closes a stem or passthrough block."
  (delimiter-line-p line +stem-delimiter+))

(defun block-attribute-line (line)
  "Return T and the attribute values when LINE is a [...] attribute line.

Every block attribute goes through here rather than each construct testing for
its own: [source,lisp], [stem] and [pass] differ only in what the values say,
and splitting them is the same job the macros in inline.lisp already do."
  (let ((trimmed (and line (trim-whitespace line))))
    (when (and trimmed
               (>= (length trimmed) 2)
               (char= (char trimmed 0) #\[)
               (char= (char trimmed (1- (length trimmed))) #\]))
      (values t (parse-attribute-list (subseq trimmed 1 (1- (length trimmed))))))))

(defun attribute-named-p (attributes name)
  "True when the first value in ATTRIBUTES is NAME, ignoring case."
  (and attributes (string-equal (first attributes) name)))

(defun item-content (line marker)
  "Return the text of LINE after a leading MARKER, or NIL if it has none.
A marker is the character in column zero followed by a space, which is what
keeps a line like `*bold* first` a paragraph: there is no space after the
opening delimiter, so it cannot be mistaken for a bullet."
  (when (and line
             (>= (length line) 2)
             (char= (char line 0) marker)
             (char= (char line 1) #\Space))
    (trim-whitespace (subseq line 2))))

(defun unordered-item-p (line)
  "True when LINE opens an unordered list item."
  (and (item-content line #\*) t))

(defun ordered-item-p (line)
  "True when LINE opens an ordered list item."
  (and (item-content line #\.) t))

(defparameter +admonition-labels+
  '(("NOTE" . :note)
    ("TIP" . :tip)
    ("WARNING" . :warning)
    ("IMPORTANT" . :important)
    ("CAUTION" . :caution))
  "The admonition labels v1 recognises, paired with the kind each denotes.")

(defun parse-admonition-line (line)
  "Return the admonition kind and text of LINE, or NIL if it opens none.
The label is matched exactly, so a paragraph beginning \"NOTES: \" is prose."
  (when line
    (loop for (label . kind) in +admonition-labels+
          for length = (length label)
          when (and (> (length line) (1+ length))
                    (string= label line :end2 length)
                    (char= (char line length) #\:)
                    (char= (char line (1+ length)) #\Space))
            do (return (values kind (subseq line (+ length 2)))))))

(defun parse-quoted-line (line)
  "Return the text of a > prefixed LINE, or NIL when it carries no prefix.
A bare > is an empty line inside the quote. The space after the marker is
required, so a line opening with >text is ordinary prose."
  (when (and line (plusp (length line)) (char= (char line 0) #\>))
    (cond ((= (length line) 1) "")
          ((char= (char line 1) #\Space) (subseq line 2)))))

(defparameter +block-image-prefix+ "image::"
  "Prefix marking a line as the block form of the image macro.")

(defun parse-block-image-line (line)
  "Return the target and raw attribute text of an image:: line, or NIL.
The bracket has to close on the last character of the line, which is what
separates a block macro from a line of prose that merely mentions one."
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
  "Delimiter separating a term from its definition.")

(defun description-delimiter-position (line)
  "Position of the :: that opens a description item in LINE, or NIL.

The delimiter is the first :: that is followed by a space or by the end of
the line, and that has a term in front of it. That single rule is what keeps
image::path[alt] an image: a block macro puts its target hard against the
colons, and a description item never does. It also means a term may itself
contain colons, so `:date:`:: is read the way it is written."
  (loop with limit = (length line)
        for position = (search +description-delimiter+ line)
          then (search +description-delimiter+ line :start2 (1+ position))
        while position
        for after = (+ position 2)
        when (and (plusp position)
                  (or (= after limit) (char= (char line after) #\Space)))
          return position))

(defun parse-description-line (line)
  "Return the term and the definition text of LINE, or NIL if it opens none."
  (when line
    (let ((position (description-delimiter-position line)))
      (when position
        (let ((term (trim-whitespace (subseq line 0 position))))
          (when (plusp (length term))
            (values term
                    (trim-whitespace (subseq line (min (length line)
                                                       (+ position 2)))))))))))

(defun block-boundary-p (line)
  "True when LINE cannot be a continuation of the paragraph in progress."
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
  "Consume a block bounded by DELIMITER, returning its text and opening line."
  (let ((opened-on (current-line-number reader)))
    (next-line reader)                  ; the opening delimiter
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
      (next-line reader)                ; the closing delimiter
      (values (format nil "~{~a~^~%~}" (nreverse collected)) opened-on))))

(defun read-listing (reader attributes)
  "Consume a listing block from READER, tagged from ATTRIBUTES if [source,lang]."
  (multiple-value-bind (text opened-on) (read-delimited-body reader +listing-delimiter+)
    (make-listing :line opened-on
                  :language (when (attribute-named-p attributes "source")
                              (second attributes))
                  :text text)))

(defun read-stem-or-passthrough (reader attributes)
  "Consume a ++++ block from READER.

The delimiter serves two constructs. [pass] asks for the body to reach the
output untouched; anything else, including no attribute line at all, is
mathematics. Asciidoctor defaults the other way, treating a bare ++++ as
passthrough -- cl-adoc does not, because math is the reason this project
chose AsciiDoc, and raw markup is the rarer thing to want."
  (multiple-value-bind (text opened-on) (read-delimited-body reader +stem-delimiter+)
    (if (attribute-named-p attributes "pass")
        (make-passthrough :line opened-on :text text)
        (make-block-stem :line opened-on :text text))))

(defun read-paragraph (reader)
  "Consume a paragraph from READER, ending at the first line that opens a block."
  (let ((line-number (current-line-number reader))
        (collected '()))
    (loop for line = (peek-line reader)
          until (or (null line) (block-boundary-p line))
          do (push (next-line reader) collected))
    (make-paragraph :line line-number
                    :content (parse-inline (format nil "~{~a~^~%~}" (nreverse collected))))))

(defun read-admonition (reader)
  "Consume an admonition from READER, including any lines it wraps onto."
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
  "Group NUMBERED-LINES, each a line number consed to its text, into paragraphs.
A blank line ends the paragraph in progress, which is how a bare > inside a
quotation separates one paragraph from the next."
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
  "Consume a run of > prefixed lines from READER as one quote."
  (let ((line-number (current-line-number reader))
        (collected '()))
    (loop for text = (parse-quoted-line (peek-line reader))
          while text
          do (push (cons (current-line-number reader) text) collected)
             (next-line reader))
    (make-blockquote :line line-number
                     :content (quoted-paragraphs (nreverse collected)))))

(defun read-block-image (reader)
  "Consume an image:: line from READER."
  (let ((line-number (current-line-number reader)))
    (multiple-value-bind (target attributes) (parse-block-image-line (next-line reader))
      (make-block-image :line line-number
                        :target target
                        :alt (first (parse-attribute-list attributes))))))

(defun parse-checkbox (text)
  "Return the checkbox state at the start of TEXT and the text following it.
The state is :CHECKED, :UNCHECKED, or NIL when TEXT opens with no checkbox."
  (when (and (>= (length text) 3)
             (char= (char text 0) #\[)
             (char= (char text 2) #\])
             ;; The bracket group has to be a word of its own, so that a line
             ;; beginning "[x]y" stays literal text.
             (or (= (length text) 3)
                 (char= (char text 3) #\Space)))
    (let ((rest (trim-whitespace (subseq text 3))))
      (case (char text 1)
        (#\Space (values :unchecked rest))
        ((#\x #\X) (values :checked rest))))))

(defun split-checkbox (text)
  "Split TEXT into its checkbox state and remaining content."
  ;; A backslash before the bracket keeps it literal. This position is the one
  ;; place checklist syntax is recognised, so it is the one place an author has
  ;; to escape a bracket they meant literally.
  (if (and (> (length text) 1)
           (char= (char text 0) #\\)
           (char= (char text 1) #\[))
      (values nil (subseq text 1))
      (multiple-value-bind (state rest) (parse-checkbox text)
        (if state
            (values state rest)
            (values nil text)))))

(defun read-list-item (reader marker)
  "Consume one item, including any lines it soft-wraps onto."
  (let* ((line-number (current-line-number reader))
         (collected (list (item-content (next-line reader) marker))))
    (loop for line = (peek-line reader)
          until (or (null line) (block-boundary-p line))
          ;; Continuation lines are conventionally indented to line up under
          ;; the marker. That indentation is presentation, not content.
          do (push (trim-whitespace (next-line reader)) collected))
    (multiple-value-bind (checked content)
        (split-checkbox (format nil "~{~a~^~%~}" (nreverse collected)))
      (make-list-item :line line-number
                      :checked checked
                      :content (parse-inline content)))))

(defun read-list (reader ordered)
  "Consume a run of list items from READER as a single list."
  (let ((line-number (current-line-number reader))
        (marker (if ordered #\. #\*))
        (items '()))
    (loop
      (push (read-list-item reader marker) items)
      ;; Items may be separated by blank lines and still belong to one list.
      ;; Any blanks consumed here would have been skipped by the block loop
      ;; regardless, so there is nothing to hand back when the list does end.
      (skip-blank-lines reader)
      (unless (item-content (peek-line reader) marker)
        (return)))
    (let ((items (nreverse items)))
      (if ordered
          (make-ordered-list :line line-number :items items)
          (make-unordered-list :line line-number :items items)))))

(defun read-description-item (reader)
  "Consume one term and its definition, including any soft-wrapped lines."
  (let ((line-number (current-line-number reader)))
    (multiple-value-bind (term definition)
        (parse-description-line (next-line reader))
      (let ((collected (if (plusp (length definition)) (list definition) '())))
        ;; A definition continues the same way a list item does, so a term may
        ;; be followed by nothing and have its definition on the lines below.
        (loop for line = (peek-line reader)
              until (or (null line) (block-boundary-p line))
              do (push (trim-whitespace (next-line reader)) collected))
        (make-description-item
         :line line-number
         :term (parse-inline term)
         :definition (parse-inline
                      (format nil "~{~a~^~%~}" (nreverse collected))))))))

(defun read-description-list (reader)
  "Consume a run of description items from READER as a single list."
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
  "Return the name and value of a :key: value attribute line, or NIL.
The name is returned as the first value and the value as the second."
  (when (and (plusp (length line)) (char= (char line 0) #\:))
    (let ((closing (position #\: line :start 1)))
      (when (and closing (> closing 1))
        (values (subseq line 1 closing)
                (trim-whitespace (subseq line (1+ closing))))))))

(defun read-header (reader)
  "Consume the document header from READER.
Returns the title as a list of inline nodes and the attributes as a hash table.
The header is the optional title line plus any attribute entries that follow
it, and it ends at the first line that is neither."
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
  "Consume every remaining block from READER."
  (let ((blocks '())
        (pending-attributes nil))
    (loop
      (skip-blank-lines reader)
      (let ((line (peek-line reader)))
        (when (null line)
          (return))
        (multiple-value-bind (attribute-line values) (block-attribute-line line)
          (if attribute-line
              ;; An attribute line configures the block that follows it, so it
              ;; is remembered rather than turned into a node of its own.
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
                            ;; After the block image, whose target sits hard
                            ;; against its own colons and so never matches.
                            ((parse-description-line line)
                             (read-description-list reader))
                            (t
                             (read-paragraph reader)))
                      blocks)
                ;; An attribute only ever applies to the block directly after it.
                (setf pending-attributes nil))))))
    (nreverse blocks)))
