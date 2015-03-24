emacs ?= emacs
all: test

test: clean
	$(emacs) -Q -batch -l tiny-test.el -l tiny.el --eval "(ert t)"

compile:
	$(emacs) -Q -batch -f batch-byte-compile tiny.el

clean:
	rm -f f.elc

.PHONY:	all test
