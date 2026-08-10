;;;; Query functions for parsed documents.
;;;;
;;;; Parsing creates a tree. Rendering consumes a tree.
;;;;
;;;; Other tasks also need the tree. For example, a caller can build a table of
;;;; contents, check whether a page needs a math renderer, or find broken image
;;;; targets.
;;;;
;;;; Those tasks all need traversal.
;;;;
;;;; Traversal belongs here because it must know every node type. If each caller
;;;; writes its own traversal, it can miss new node types when cl-adoc grows.

(in-package #:adoc)

(defgeneric node-children (node)
  (:documentation
   "Return the child nodes of NODE.

Return NIL when NODE has no children.

This is the extension point for traversal. A new container node needs a method
here. A new leaf node needs no method.")
  (:method (node)
    (declare (ignore node))
    nil))

(defmethod node-children ((node document))
  (append (document-title node) (document-blocks node)))

(defmethod node-children ((node heading)) (heading-content node))
(defmethod node-children ((node paragraph)) (paragraph-content node))
(defmethod node-children ((node admonition)) (admonition-content node))
(defmethod node-children ((node blockquote)) (blockquote-content node))
(defmethod node-children ((node unordered-list)) (unordered-list-items node))
(defmethod node-children ((node ordered-list)) (ordered-list-items node))
(defmethod node-children ((node list-item)) (list-item-content node))
(defmethod node-children ((node description-list))
  (description-list-items node))
(defmethod node-children ((node description-item))
  ;; Walk both the term and the definition.
  ;;
  ;; A term can contain inline nodes, such as links and code spans. Walking
  ;; only the definition would hide part of the document from callers.
  (append (description-item-term node) (description-item-definition node)))
(defmethod node-children ((node strong)) (strong-content node))
(defmethod node-children ((node emphasis)) (emphasis-content node))
(defmethod node-children ((node link)) (link-content node))

(defun map-nodes (function root)
  "Call FUNCTION on ROOT and each node below it, in document order.

ROOT can be one node or a list of nodes."
  (if (listp root)
      (dolist (each root)
        (map-nodes function each))
      (progn
        (funcall function root)
        (dolist (child (node-children root))
          (map-nodes function child))))
  (values))

(defun collect-nodes (predicate root)
  "Return all nodes at or below ROOT that satisfy PREDICATE.

Nodes are returned in document order."
  (let ((found '()))
    (map-nodes (lambda (node)
                 (when (funcall predicate node)
                   (push node found)))
               root)
    (nreverse found)))

(defun find-node (predicate root)
  "Return the first node at or below ROOT that satisfies PREDICATE.

Return NIL when no node matches."
  (block searching
    (map-nodes (lambda (node)
                 (when (funcall predicate node)
                   (return-from searching node)))
               root)
    nil))

(defun node-text (root)
  "Return the text of ROOT with markup removed.

An image contributes its alt text. That is the image text available to the
document."
  (with-output-to-string (out)
    (map-nodes (lambda (node)
                 (typecase node
                   (text (write-string (text-string node) out))
                   (monospace (write-string (monospace-string node) out))
                   (inline-stem (write-string (inline-stem-text node) out))
                   (inline-image (write-string (or (inline-image-alt node) "") out))))
               root)))

;;; Identifiers

(defun slugify (string)
  "Reduce STRING to a lowercase fragment identifier.

Runs of non-alphanumeric characters become one hyphen. Leading and trailing
hyphens are removed."
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

(defun heading-id (heading)
  "Return the generated identifier for HEADING.

This function is exported and used by the HTML backend. A table of contents can
therefore use the same anchors as the renderer."
  (slugify (node-text heading)))

;;; Document-level queries

(defun document-headings (document)
  "Return every heading in DOCUMENT, in document order."
  (collect-nodes #'heading-p document))

(defun document-uses-stem-p (document)
  "Return true when DOCUMENT contains mathematics.

This function checks the full tree. Inline mathematics can appear inside a
paragraph, so scanning only top-level blocks is not sufficient."
  (and (find-node (lambda (node)
                    (or (block-stem-p node) (inline-stem-p node)))
                  document)
       t))

(defun document-references (document)
  "Return every node in DOCUMENT that names an external target.

This includes links, inline images, and block images."
  (collect-nodes (lambda (node)
                   (or (link-p node)
                       (inline-image-p node)
                       (block-image-p node)))
                 document))

(defun reference-target (node)
  "Return the target named by a link or image NODE."
  (etypecase node
    (link (link-target node))
    (inline-image (inline-image-target node))
    (block-image (block-image-target node))))
