;;;; The public interface.
;;;;
;;;; Parsing and rendering are separate calls on purpose. A caller that wants
;;;; the attributes of a document, or a table of contents, or its word count,
;;;; should not have to render it to get them.

(in-package #:adoc)

(defun parse-string (string)
  "Parse STRING as an AsciiDoc document and return a DOCUMENT."
  (let ((reader (make-line-reader string)))
    (multiple-value-bind (title attributes) (read-header reader)
      (make-document :title title
                     :attributes attributes
                     :blocks (read-blocks reader)))))

(defun parse-file (path)
  "Parse the file at PATH as an AsciiDoc document and return a DOCUMENT."
  (parse-string (uiop:read-file-string path)))

(defun render-html (document &optional stream)
  "Render DOCUMENT as an HTML5 fragment.
Writes to STREAM when one is supplied and returns NIL; otherwise returns the
fragment as a string."
  (let ((backend (make-instance 'html5)))
    (if stream
        (render-node document backend stream)
        (with-output-to-string (out)
          (render-node document backend out)))))
