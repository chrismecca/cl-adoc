;;;; Reading a parsed document.
;;;;
;;;; Parsing produces a tree; rendering consumes one. Everything else a caller
;;;; wants to do -- build a table of contents, decide whether a page needs a
;;;; mathematics renderer loaded, check that every image on a page actually
;;;; exists -- is the same operation underneath: walk the tree and collect.
;;;;
;;;; That walk belongs here rather than in each caller, because it has to know
;;;; every node type. A caller that writes its own breaks quietly the next time
;;;; cl-adoc learns a construct, and the way it breaks is by silently walking
;;;; less of the document than it used to.

(in-package #:adoc)

(defgeneric node-children (node)
  (:documentation
   "Return the child nodes of NODE, or NIL when it has none.
This is the extension point for the traversal below: a new container type needs
a method here, and a new leaf type needs nothing.")
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
  ;; A term can hold a link or a code span as readily as a definition can, so
  ;; both halves are walked. Returning only the definition would hide half of
  ;; every description list from every caller.
  (append (description-item-term node) (description-item-definition node)))
(defmethod node-children ((node strong)) (strong-content node))
(defmethod node-children ((node emphasis)) (emphasis-content node))
(defmethod node-children ((node link)) (link-content node))

(defun map-nodes (function root)
  "Call FUNCTION on ROOT and on every node beneath it, in document order.
ROOT may be a single node or a list of them, since several slots hold lists."
  (if (listp root)
      (dolist (each root)
        (map-nodes function each))
      (progn
        (funcall function root)
        (dolist (child (node-children root))
          (map-nodes function child))))
  (values))

(defun collect-nodes (predicate root)
  "Return every node at or beneath ROOT satisfying PREDICATE, in document order."
  (let ((found '()))
    (map-nodes (lambda (node)
                 (when (funcall predicate node)
                   (push node found)))
               root)
    (nreverse found)))

(defun find-node (predicate root)
  "Return the first node at or beneath ROOT satisfying PREDICATE, or NIL."
  (block searching
    (map-nodes (lambda (node)
                 (when (funcall predicate node)
                   (return-from searching node)))
               root)
    nil))

(defun node-text (root)
  "Return the text of ROOT with all markup removed.
An image contributes its alt text, which is the only part of it that is words."
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
  "Reduce STRING to a lowercase identifier suitable for a fragment link.
Runs of characters that are neither letters nor digits collapse to a single
hyphen, and leading and trailing hyphens are dropped."
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

Exported, and used by the HTML backend rather than duplicated inside it: a
table of contents has to emit exactly the anchors the renderer emits, and
deriving them in two places is how the two drift apart."
  (slugify (node-text heading)))

;;; Questions worth asking of a whole document

(defun document-headings (document)
  "Return every heading in DOCUMENT, in document order."
  (collect-nodes #'heading-p document))

(defun document-uses-stem-p (document)
  "True when DOCUMENT contains mathematics anywhere in it.

This is worth a function of its own because the obvious version is wrong.
Inline mathematics lives inside a paragraph's content, so a caller scanning
only the top-level blocks finds every display equation and misses every
expression written in the middle of a sentence."
  (and (find-node (lambda (node)
                    (or (block-stem-p node) (inline-stem-p node)))
                  document)
       t))

(defun document-references (document)
  "Return every node in DOCUMENT that names an external target.
That is links and both forms of image, which together are what a build has to
check before it can claim a page has no broken references."
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
