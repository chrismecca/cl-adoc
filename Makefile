# cl-adoc

# --noinform is an SBCL runtime option.  Put it before the toplevel options.
SBCL := sbcl
LISP := $(SBCL) --noinform --non-interactive

.PHONY: all build test repl clean

all: test

build:
	$(LISP) --eval '(asdf:load-system :cl-adoc)'

test:
	$(LISP) --eval '(asdf:test-system :cl-adoc)'

repl:
	$(SBCL) --eval '(asdf:load-system :cl-adoc)'

# ASDF stores compiled output outside this tree.
# Clean only the ASDF cache for this project.
clean:
	rm -rf $$HOME/.config/cache/common-lisp/*/$(CURDIR)
