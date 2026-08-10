;;;; A cursor over document lines.
;;;;
;;;; The block grammar is line-based. Delimiters occupy a full line. Blank
;;;; lines separate blocks. Attribute lines apply to the block that follows.
;;;;
;;;; The reader therefore stores lines, not characters. This matches the
;;;; grammar and keeps line numbers available for error reports.

(in-package #:adoc)

(defstruct (line-reader (:constructor %make-line-reader (lines)))
  (lines #() :type simple-vector)
  (index 0 :type fixnum))

(defun strip-carriage-return (line)
  "Remove one trailing carriage return from LINE.

This accepts CRLF input. Later parser code does not need to know which line
ending the document used."
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
    ;; A trailing newline ends the last line. It does not start an empty line.
    (when (< start (length string))
      (vector-push-extend (strip-carriage-return (subseq string start)) lines))
    (coerce lines 'simple-vector)))

(defun make-line-reader (string)
  "Return a LINE-READER positioned at the first line of STRING."
  (%make-line-reader (split-lines string)))

(defun exhausted-p (reader)
  "Return true when READER has no lines left."
  (>= (line-reader-index reader) (length (line-reader-lines reader))))

(defun peek-line (reader)
  "Return the current line without consuming it.

Return NIL at end of input."
  (unless (exhausted-p reader)
    (svref (line-reader-lines reader) (line-reader-index reader))))

(defun next-line (reader)
  "Consume and return the current line.

Return NIL at end of input."
  (let ((line (peek-line reader)))
    (when line
      (incf (line-reader-index reader)))
    line))

(defun current-line-number (reader)
  "Return the one-based number of the current line."
  (1+ (line-reader-index reader)))

(defun blank-line-p (line)
  "Return true when LINE is NIL or only spaces and tabs."
  (or (null line)
      (every (lambda (character)
               (member character '(#\Space #\Tab)))
             line)))

(defun skip-blank-lines (reader)
  "Advance READER past blank lines."
  (loop while (and (not (exhausted-p reader))
                   (blank-line-p (peek-line reader)))
        do (next-line reader)))

(defun trim-whitespace (string)
  "Return STRING without leading or trailing spaces and tabs."
  (string-trim '(#\Space #\Tab) string))
