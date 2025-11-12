CLOJURE=clojure

.PHONY: dev test demo repl

dev:
	./scripts/dev.sh

test:
	$(CLOJURE) -M:test

demo:
	$(CLOJURE) -M:demo

repl:
	@ADMIN_TOKEN=$${ADMIN_TOKEN:-$$([ -f .admintoken ] && cat .admintoken || echo change-me)}; \
	$(CLOJURE) -M -e "(require,'repl.http) (repl.http/start! {:port 6767 :bind \"127.0.0.1\" :token \"$$ADMIN_TOKEN\"})" -r
