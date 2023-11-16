EMACS := emacs

all:
	$(EMACS) --batch --quick \
		--directory . \
		--load syncthing-tests.el \
		--funcall ert-run-tests-batch
