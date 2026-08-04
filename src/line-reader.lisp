;;;; A cursor over the lines of a document.
;;;;
;;;; AsciiDoc's block grammar is stated in terms of lines: delimiters occupy a
;;;; line of their own, blocks are separated by blank lines, and an attribute
;;;; line binds to whatever block follows it. Reading the document as a
;;;; sequence of lines rather than a sequence of characters is not a shortcut
;;;; around the grammar -- it is the shape the grammar is written in, and it
;;;; keeps an accurate line number available for error reporting at no cost.

(in-package #:adoc)

(defstruct (line-reader (:constructor %make-line-reader (lines)))
  (lines #() :type simple-vector)
  (index 0 :type fixnum))

(defun strip-carriage-return (line)
  "Remove a single trailing carriage return from LINE, if present.
Documents written on Windows arrive with CRLF endings; nothing downstream
should have to care."
  (let ((length (length line)))
    (if (and (plusp length) (char= (char line (1- length)) #\Return))
        (subseq line 0 (1- length))
        line)))

(defun split-lines (string)
  "Split STRING into a simple vector of lines."
  (let ((lines (make-array 0 :adjustable t :fill-pointer t))
        (start 0))
    (dotimes (i (length string))
      (when (char= (char string i) #\Newline)
        (vector-push-extend (strip-carriage-return (subseq string start i)) lines)
        (setf start (1+ i))))
    ;; A trailing newline ends the last line; it does not begin an empty one.
    (when (< start (length string))
      (vector-push-extend (strip-carriage-return (subseq string start)) lines))
    (coerce lines 'simple-vector)))

(defun make-line-reader (string)
  "Return a LINE-READER positioned at the first line of STRING."
  (%make-line-reader (split-lines string)))

(defun exhausted-p (reader)
  "True when READER has no lines left."
  (>= (line-reader-index reader) (length (line-reader-lines reader))))

(defun peek-line (reader)
  "Return the current line without consuming it, or NIL at end of input."
  (unless (exhausted-p reader)
    (svref (line-reader-lines reader) (line-reader-index reader))))

(defun next-line (reader)
  "Consume and return the current line, or NIL at end of input."
  (let ((line (peek-line reader)))
    (when line
      (incf (line-reader-index reader)))
    line))

(defun current-line-number (reader)
  "Return the one-based number of the line READER is positioned on."
  (1+ (line-reader-index reader)))

(defun blank-line-p (line)
  "True when LINE is NIL or contains nothing but whitespace."
  (or (null line)
      (every (lambda (character)
               (member character '(#\Space #\Tab)))
             line)))

(defun skip-blank-lines (reader)
  "Advance READER past any run of blank lines."
  (loop while (and (not (exhausted-p reader))
                   (blank-line-p (peek-line reader)))
        do (next-line reader)))

(defun trim-whitespace (string)
  "Return STRING without leading or trailing spaces and tabs."
  (string-trim '(#\Space #\Tab) string))
