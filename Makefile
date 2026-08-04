# cl-adoc

# --noinform is a runtime option, so it has to precede the toplevel options.
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

# ASDF caches compiled output outside the tree, so there is nothing to remove
# here beyond that cache.
clean:
	rm -rf $$HOME/.config/cache/common-lisp/*/$(CURDIR)
