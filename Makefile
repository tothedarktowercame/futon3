CLOJURE=clojure

.PHONY: dev test test-real demo repl lint sync-patterns

dev:
	./scripts/dev.sh

test:
	$(CLOJURE) -M:test
	./scripts/test-elisp.sh

# Integration test suite that hits a running Futon1 API (usually futon1a via `make dev`)
test-real:
	FUTON3_REAL_DATA_TESTS=1 FUTON1_API_BASE=$${FUTON1_API_BASE:-http://localhost:8080} $(CLOJURE) -X:test :nses '[futon3.real-client-interfaces-test]'

demo:
	$(CLOJURE) -M:demo

repl:
	@ADMIN_TOKEN=$${ADMIN_TOKEN:-$$([ -f .admintoken ] && tr -d '\n' < .admintoken || echo change-me)}; \
	$(CLOJURE) -M -e "(require,'repl.http) (repl.http/start! {:port 6767 :bind \"127.0.0.1\" :token \"$$ADMIN_TOKEN\"})" -r

lint:
	./scripts/lint.sh

sync-patterns:
	./scripts/sync_patterns.sh
