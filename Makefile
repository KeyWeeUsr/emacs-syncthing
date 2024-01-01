EMACS := emacs
EMACSCLIENT := emacsclient
DEMO_HOST := 127.0.0.1
DEMO_PORT := 5000
DEMO_PROTO := http
DEMO_ADDR := $(DEMO_PROTO)://$(DEMO_HOST):$(DEMO_PORT)
DEMO_TOKEN := dummy

.PHONY: all
all: test

.PHONY: clean
clean:
	@-rm syncthing*.elc 2>/dev/null
	@-rm syncthing*.ok 2>/dev/null

%.elc: %.el
	@-rm "$@" 2>/dev/null
	@$(EMACS) --batch --quick \
		--directory . \
		--load compile-setup \
		--eval '(byte-compile-file "$(subst .elc,.el,$@)")' \
		&& test -f "$@"

.PHONY: byte-compile
byte-compile: \
	syncthing-common.elc \
	syncthing-common-tests.elc \
	syncthing-constants.elc \
	syncthing-custom.elc \
	syncthing-draw.elc \
	syncthing.elc \
	syncthing-errors.elc \
	syncthing-faces.elc \
	syncthing-groups.elc \
	syncthing-keyboard.elc \
	syncthing-keyboard-tests.elc \
	syncthing-network.elc \
	syncthing-network-tests.elc \
	syncthing-state.elc \
	syncthing-tests.elc \
	syncthing-update.elc \
	syncthing-watcher.elc

.PHONY: test
test: byte-compile main-tests keyboard-tests common-tests network-tests

syncthing-network-tests.ok: syncthing-network.elc syncthing-network-tests.elc
	@$(EMACS) --batch --quick \
		--directory . \
		--load syncthing-network-tests.el \
		--funcall ert-run-tests-batch-and-exit \
	&& touch syncthing-network-tests.ok
network-tests: syncthing-network-tests.ok

syncthing-common-tests.ok: syncthing-common.elc syncthing-common-tests.elc
	@$(EMACS) --batch --quick \
		--directory . \
		--load syncthing-common-tests.el \
		--funcall ert-run-tests-batch-and-exit \
	&& touch syncthing-common-tests.ok
common-tests: syncthing-common-tests.ok

syncthing-keyboard-tests.ok: syncthing-keyboard.elc syncthing-keyboard-tests.elc
	@$(EMACS) --batch --quick \
		--directory . \
		--load syncthing-keyboard-tests.el \
		--funcall ert-run-tests-batch-and-exit \
	&& touch syncthing-keyboard-tests.ok
keyboard-tests: syncthing-keyboard-tests.ok

syncthing-tests.ok: syncthing.elc syncthing-tests.elc
	@$(EMACS) --batch --quick \
		--directory . \
		--load syncthing-tests.el \
		--funcall ert-run-tests-batch-and-exit \
	&& touch syncthing-tests.ok
main-tests: syncthing-tests.ok

.PHONY: demo-server
demo-server:
	FLASK_APP=demo/demo.py flask run \
		--host $(DEMO_HOST) --port $(DEMO_PORT) \
		--reload

.PHONY: demo
demo:
	$(EMACSCLIENT) --eval \
		'(load "$(PWD)/demo/demo.el")' \
		'(syncthing-demo "Demo" "$(DEMO_ADDR)")' &
	$(MAKE) demo-server

Makefile.ok: Makefile
	@make -n all
	@docker run \
		--network=none \
		--volume "$(PWD)"/Makefile:/Makefile \
		backplane/checkmake /Makefile
lint-makefile: Makefile.ok

.PHONY: tag
tag:
	$(MAKE) all
	git add -f . && git stash
	@grep ";; Version:" syncthing.el | tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
