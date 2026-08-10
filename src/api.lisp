;;;; The public interface.
;;;;
;;;; Parsing and rendering are separate operations.
;;;;
;;;; A caller can inspect a document without rendering it. For example, it can
;;;; read attributes, build a table of contents, or count words.

(in-package #:adoc)

(defun parse-string (string)
  "Parse STRING as an AsciiDoc document.

Return a DOCUMENT."
  (let ((reader (make-line-reader string)))
    (multiple-value-bind (title attributes) (read-header reader)
      (make-document :title title
                     :attributes attributes
                     :blocks (read-blocks reader)))))

(defun parse-file (path)
  "Parse the file at PATH as an AsciiDoc document.

Return a DOCUMENT."
  (parse-string (uiop:read-file-string path)))

(defun render-html (document &optional stream)
  "Render DOCUMENT as an HTML5 fragment.

When STREAM is supplied, write to STREAM and return NIL.

When STREAM is not supplied, return the fragment as a string."
  (let ((backend (make-instance 'html5)))
    (if stream
        (render-node document backend stream)
        (with-output-to-string (out)
          (render-node document backend out)))))
