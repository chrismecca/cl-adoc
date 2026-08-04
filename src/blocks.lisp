;;;; The block layer.
;;;;
;;;; One pass over the line reader. Each iteration looks at the current line,
;;;; decides which construct it opens, and hands the reader to a function that
;;;; consumes exactly that construct. Nothing backtracks, so a malformed
;;;; document fails at the line that caused it rather than somewhere later.

(in-package #:adoc)

(defparameter +listing-delimiter+ "----"
  "Delimiter line opening and closing a listing block.")

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

(defun listing-delimiter-p (line)
  "True when LINE opens or closes a listing block."
  (and line (string= (trim-whitespace line) +listing-delimiter+)))

(defun source-attribute-line-p (line)
  "Return the language of a [source] attribute line, T if it names none, or NIL.
Returning T rather than a language keeps the caller's contract simple: a true
value means the next block is source, and a string means it is tagged."
  (let ((trimmed (trim-whitespace line)))
    (when (and (> (length trimmed) 2)
               (char= (char trimmed 0) #\[)
               (char= (char trimmed (1- (length trimmed))) #\]))
      (let* ((body (subseq trimmed 1 (1- (length trimmed))))
             (comma (position #\, body)))
        (cond ((not (string-equal (trim-whitespace (subseq body 0 comma)) "source"))
               nil)
              (comma
               (let ((language (trim-whitespace (subseq body (1+ comma)))))
                 (if (plusp (length language)) language t)))
              (t t))))))

(defun block-boundary-p (line)
  "True when LINE cannot be a continuation of the paragraph in progress."
  (or (blank-line-p line)
      (heading-line-p line)
      (listing-delimiter-p line)
      (and (source-attribute-line-p line) t)))

;;; Individual constructs

(defun read-heading (reader)
  "Consume a heading from READER."
  (let* ((line-number (current-line-number reader))
         (line (next-line reader))
         (level (heading-line-p line)))
    (make-heading :level level
                  :line line-number
                  :content (parse-inline (trim-whitespace (subseq line level))))))

(defun read-listing (reader language)
  "Consume a delimited listing block from READER, tagged with LANGUAGE."
  (let ((opened-on (current-line-number reader)))
    (next-line reader)                  ; the opening delimiter
    (let ((collected '()))
      (loop for line = (peek-line reader)
            until (or (null line) (listing-delimiter-p line))
            do (push (next-line reader) collected)
            finally (when (null line)
                      (error 'unterminated-block
                             :line opened-on
                             :delimiter +listing-delimiter+
                             :message (format nil "Listing block opened with ~a was never closed"
                                              +listing-delimiter+))))
      (next-line reader)                ; the closing delimiter
      (make-listing :line opened-on
                    :language (when (stringp language) language)
                    :text (format nil "~{~a~^~%~}" (nreverse collected))))))

(defun read-paragraph (reader)
  "Consume a paragraph from READER, ending at the first line that opens a block."
  (let ((line-number (current-line-number reader))
        (collected '()))
    (loop for line = (peek-line reader)
          until (or (null line) (block-boundary-p line))
          do (push (next-line reader) collected))
    (make-paragraph :line line-number
                    :content (parse-inline (format nil "~{~a~^~%~}" (nreverse collected))))))

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
        (pending-source nil))
    (loop
      (skip-blank-lines reader)
      (let ((line (peek-line reader)))
        (when (null line)
          (return))
        (let ((source (source-attribute-line-p line)))
          (if source
              ;; An attribute line configures the block that follows it, so it
              ;; is remembered rather than turned into a node of its own.
              (progn (setf pending-source source)
                     (next-line reader))
              (progn
                (push (cond ((listing-delimiter-p line)
                             (read-listing reader pending-source))
                            ((heading-line-p line)
                             (read-heading reader))
                            (t
                             (read-paragraph reader)))
                      blocks)
                ;; An attribute only ever applies to the block directly after it.
                (setf pending-source nil))))))
    (nreverse blocks)))
