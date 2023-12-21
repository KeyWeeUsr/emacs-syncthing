EMACS := emacs

all:
	@-rm syncthing.elc 2>/dev/null
	$(EMACS) --batch --quick \
		--directory . \
		--load syncthing-tests.el \
		--funcall ert-run-tests-batch
