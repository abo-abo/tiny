emacs ?= emacs
BEMACS = $(emacs) -batch -l targets/elpa.el

all: test

update:
	$(emacs) -batch -l targets/install-deps.el

# Use LC_ALL=C to avoid locale dependencies in the dates!
test: clean
	LC_ALL=C $(BEMACS) -l tiny-test.el -l tiny.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -Q -batch -f batch-byte-compile tiny.el

clean:
	rm -f f.elc

.PHONY:	all test
