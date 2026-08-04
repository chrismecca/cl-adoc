;;;; Golden-file tests.
;;;;
;;;; Each fixture is a pair: NAME.adoc holds the input and NAME.html holds the
;;;; output it is expected to produce. Adding a case means adding two files and
;;;; no code, which is the point -- the corpus is meant to grow every time the
;;;; parser learns a construct or gets a bug report.
;;;;
;;;; The expected files are checked in and reviewed like any other source. They
;;;; are not regenerated as part of a test run: a golden file that rewrites
;;;; itself when the output changes records whatever the parser did last, which
;;;; is the opposite of what it is for.

(in-package #:adoc/tests)

(defun fixture-directory ()
  (asdf:system-relative-pathname "cl-adoc" "tests/fixtures/"))

(defun fixture-inputs ()
  "Return every fixture input, ordered by name for a stable report."
  (sort (directory (merge-pathnames "*.adoc" (fixture-directory)))
        #'string< :key #'pathname-name))

(defun trailing-whitespace-trimmed (string)
  "Return STRING without trailing whitespace.
Keeps the comparison from turning into an argument about how an editor saves
the last line of a file."
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
