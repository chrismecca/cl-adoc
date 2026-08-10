;;;; Golden-file tests.
;;;;
;;;; Each fixture has two files:
;;;;
;;;; * NAME.adoc is the input.
;;;; * NAME.html is the expected output.
;;;;
;;;; To add a case, add both files. No test code must change.
;;;;
;;;; The fixture set should grow when the parser gets a feature or a bug fix.
;;;;
;;;; Expected output files are source files. Review them like other source
;;;; files. Do not regenerate them during a test run. A golden file that updates
;;;; itself records the parser's last output, not the correct output.

(in-package #:adoc/tests)

(defun fixture-directory ()
  (asdf:system-relative-pathname "cl-adoc" "tests/fixtures/"))

(defun fixture-inputs ()
  "Return all fixture inputs.

Sort them by name so test reports are stable."
  (sort (directory (merge-pathnames "*.adoc" (fixture-directory)))
        #'string< :key #'pathname-name))

(defun trailing-whitespace-trimmed (string)
  "Return STRING without trailing whitespace.

This keeps the comparison independent of editor behavior at the end of a file."
  (string-right-trim '(#\Space #\Tab #\Newline #\Return) string))

(define-test golden)

(define-test (golden fixtures)
  (let ((inputs (fixture-inputs)))
    (true (plusp (length inputs)) "The fixture directory is not empty.")
    (dolist (input inputs)
      (let* ((name (pathname-name input))
             (expected-path (make-pathname :type "html" :defaults input)))
        (if (probe-file expected-path)
            (is string=
                (trailing-whitespace-trimmed (uiop:read-file-string expected-path))
                (trailing-whitespace-trimmed (adoc:render-html (adoc:parse-file input)))
                "fixture ~a" name)
            (true nil "fixture ~a has no expected output at ~a" name expected-path))))))
