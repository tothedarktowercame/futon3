CLOJURE=clojure

.PHONY: dev test demo repl lint

dev:
	./scripts/dev.sh

test:
	$(CLOJURE) -M:test
	./scripts/test-elisp.sh

demo:
	$(CLOJURE) -M:demo

repl:
	@ADMIN_TOKEN=$${ADMIN_TOKEN:-$$([ -f .admintoken ] && tr -d '\n' < .admintoken || echo change-me)}; \
	$(CLOJURE) -M -e "(require,'repl.http) (repl.http/start! {:port 6767 :bind \"127.0.0.1\" :token \"$$ADMIN_TOKEN\"})" -r

lint:
	./scripts/lint.sh
