test:
	emacs -batch -l treesitedit.el -l treesitedit-tests.el -f ert-run-tests-batch-and-exit

.PHONY: test
