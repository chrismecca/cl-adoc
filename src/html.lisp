;;;; The HTML5 backend.
;;;;
;;;; Rendering is a generic function taking the node and a backend object, so
;;;; a second output format is a new set of methods rather than an edit to an
;;;; existing one. The backend argument is doing real work even with only one
;;;; backend defined: it is what keeps the tree in ast.lisp from acquiring
;;;; HTML-specific slots the moment something else needs rendering.
;;;;
;;;; This file is also the only place escaping happens, and it happens exactly
;;;; once, on text that has never been anywhere near a markup string.

(in-package #:adoc)

(defclass backend () ()
  (:documentation "Base class for output formats."))

(defclass html5 (backend) ()
  (:documentation "Renders a document as an HTML5 fragment."))

(defgeneric render-node (node backend stream)
  (:documentation "Write NODE to STREAM in the format defined by BACKEND."))

(defun escape-text (string stream)
  "Write STRING to STREAM with the three characters that are markup in HTML
text content replaced by entities."
  (loop for character across string
        do (case character
             (#\& (write-string "&amp;" stream))
             (#\< (write-string "&lt;" stream))
             (#\> (write-string "&gt;" stream))
             (t (write-char character stream)))))

(defun escape-attribute (string stream)
  "Write STRING to STREAM escaped for use inside a double-quoted attribute."
  (loop for character across string
        do (case character
             (#\& (write-string "&amp;" stream))
             (#\< (write-string "&lt;" stream))
             (#\> (write-string "&gt;" stream))
             (#\" (write-string "&quot;" stream))
             (t (write-char character stream)))))

(defun slugify (string)
  "Reduce STRING to a lowercase identifier suitable for a fragment link.
Runs of characters that are not letters or digits collapse to a single hyphen,
and leading and trailing hyphens are dropped."
  (let ((slug (make-string-output-stream))
        (pending-hyphen nil)
        (seen-content nil))
    (loop for character across string
          do (if (alphanumericp character)
                 (progn
                   (when (and pending-hyphen seen-content)
                     (write-char #\- slug))
                   (write-char (char-downcase character) slug)
                   (setf pending-hyphen nil
                         seen-content t))
                 (setf pending-hyphen t)))
    (get-output-stream-string slug)))

(defun render-children (nodes backend stream)
  "Render each of NODES to STREAM in order."
  (dolist (node nodes)
    (render-node node backend stream)))

;;; Inline nodes

(defmethod render-node ((node text) (backend html5) stream)
  (escape-text (text-string node) stream))

(defmethod render-node ((node strong) (backend html5) stream)
  (write-string "<strong>" stream)
  (render-children (strong-content node) backend stream)
  (write-string "</strong>" stream))

(defmethod render-node ((node emphasis) (backend html5) stream)
  (write-string "<em>" stream)
  (render-children (emphasis-content node) backend stream)
  (write-string "</em>" stream))

(defmethod render-node ((node monospace) (backend html5) stream)
  (write-string "<code>" stream)
  (escape-text (monospace-string node) stream)
  (write-string "</code>" stream))

;;; Block nodes

(defmethod render-node ((node heading) (backend html5) stream)
  (let ((level (heading-level node))
        (id (slugify (inline-text (heading-content node)))))
    (format stream "<h~d id=\"" level)
    (escape-attribute id stream)
    (write-string "\">" stream)
    (render-children (heading-content node) backend stream)
    (format stream "</h~d>~%" level)))

(defmethod render-node ((node paragraph) (backend html5) stream)
  (write-string "<p>" stream)
  (render-children (paragraph-content node) backend stream)
  (format stream "</p>~%"))

(defmethod render-node ((node listing) (backend html5) stream)
  (write-string "<pre><code" stream)
  (let ((language (listing-language node)))
    (when language
      (write-string " class=\"language-" stream)
      (escape-attribute language stream)
      (write-string "\"" stream)))
  (write-string ">" stream)
  (escape-text (listing-text node) stream)
  (format stream "</code></pre>~%"))

;;; Documents

(defmethod render-node ((node document) (backend html5) stream)
  (let ((title (document-title node)))
    (when title
      (write-string "<h1>" stream)
      (render-children title backend stream)
      (format stream "</h1>~%")))
  (render-children (document-blocks node) backend stream))
