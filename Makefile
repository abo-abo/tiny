CASK = ~/.cask/bin/cask
emacs ?= emacs
all: test

test: clean
	$(CASK) exec emacs -Q -batch -l tiny-test.el -l tiny.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -Q -batch -f batch-byte-compile tiny.el

clean:
	rm -f f.elc

.PHONY:	all test
