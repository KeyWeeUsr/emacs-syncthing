EMACS := emacs
EMACSCLIENT := emacsclient
DEMO_HOST := 127.0.0.1
DEMO_PORT := 5000
DEMO_PROTO := http
DEMO_ADDR := $(DEMO_PROTO)://$(DEMO_HOST):$(DEMO_PORT)
DEMO_TOKEN := dummy

all:
	@-rm syncthing.elc 2>/dev/null
	$(EMACS) --batch --quick \
		--directory . \
		--load syncthing-tests.el \
		--funcall ert-run-tests-batch

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

.PHONY: tag
tag:
	git add -f . && git stash
	@grep ";; Version:" syncthing.el | tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
