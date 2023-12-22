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

demo:
	$(EMACSCLIENT) --eval \
		'(syncthing-with-base "Demo" "$(DEMO_ADDR)" "$(DEMO_TOKEN)")'
	FLASK_APP=demo.py flask run \
		--host $(DEMO_HOST) --port $(DEMO_PORT) \
		--reload
