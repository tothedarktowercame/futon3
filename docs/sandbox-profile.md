# Sandbox Profile

The sandbox profile is the canonical environment for reproducible proofwork
runs. It provides deterministic conditions for golden transcript generation
and CI verification.

## Purpose

1. **Reproducibility**: Same inputs → same outputs across machines and time
2. **Isolation**: No external dependencies or network calls
3. **Speed**: Fast execution for CI feedback loops
4. **Verification**: Golden transcripts replay identically

## Profile Configuration

### Clock

The sandbox uses a fixed clock to ensure timestamp determinism:

```clojure
{:config {:clock {:fixed "2026-01-01T00:00:00Z"}}}
```

All `(now)` calls return this instant. Time-based IDs are predictable.

### Run ID Seed

Deterministic run IDs for correlation:

```clojure
{:config {:run-id-seed "RUN-GOLDEN"}}
```

Generated IDs follow pattern: `RUN-GOLDEN-001`, `RUN-GOLDEN-002`, etc.

### Mock Adapters

External services are replaced with mock adapters:

```clojure
(require '[f2.adapters.mock :as mock])
{:adapters (atom (mock/adapters))}
```

Mock adapters provide:
- Deterministic responses
- Configurable delays (default: 0ms)
- Request/response logging for debugging

### REPL Mode

Disabled in sandbox:

```clojure
{:config {:repl {:mode :off}}}
```

No interactive sessions or Drawbridge access.

## Creating Sandbox State

```clojure
(defn make-sandbox-state []
  {:config (atom {:repl {:mode :off}
                  :run-id-seed "RUN-GOLDEN"
                  :clock {:fixed "2026-01-01T00:00:00Z"}})
   :clients (atom {"C-1" {:id "C-1"
                          :name "sandbox-client"
                          :caps #{"check" "gap-report" "trail-capture"}
                          :connected? true
                          :remote-addr "127.0.0.1"}})
   :history (atom [])
   :adapters (atom (mock/adapters))
   :router (atom nil)})
```

## Golden Transcript Generation

Golden transcripts are NDJSON files capturing request/reply pairs:

```
test/fixtures/transport/
├── golden-check-request.ndjson
├── golden-check-reply.ndjson
├── golden-gap-report-request.ndjson
├── golden-gap-report-reply.ndjson
├── golden-trail-capture-request.ndjson
└── golden-trail-capture-reply.ndjson
```

### Recording New Golden Transcripts

1. Create sandbox state
2. Execute operations
3. Serialize requests and replies as NDJSON
4. Commit as fixtures

```clojure
(let [state (make-sandbox-state)
      request {:type "check" :pattern-id "test/pattern" ...}
      envelope {:client-id "C-1" :payload request}
      result (transport/apply-envelope! state "C-1" envelope)]
  ;; Save request
  (spit "golden-check-request.ndjson"
        (str (json/encode envelope) "\n"))
  ;; Save reply
  (spit "golden-check-reply.ndjson"
        (str (json/encode (:reply result)) "\n")))
```

### Replaying Golden Transcripts

```clojure
(defn replay-transcript! [request-path reply-path]
  (let [state (make-sandbox-state)
        requests (load-ndjson request-path)
        expected (load-ndjson reply-path)
        actual (map #(apply-and-capture state %) requests)]
    (assert (= expected actual))))
```

See `test/f2/transport_golden_test.clj` for the full implementation.

## CI Integration

Golden transcript tests run in CI via GitHub Actions:

```yaml
- name: Run tests
  run: clojure -M:test -m cognitect.test-runner
```

The test suite includes:
- `f2.transport-golden-test/golden-check-transcript-replays`
- `f2.transport-golden-test/golden-gap-report-transcript-replays`
- `f2.transport-golden-test/golden-trail-capture-transcript-replays`

### Failure Modes

If a golden test fails:

1. **Intentional change**: Update the golden fixture
2. **Regression**: Fix the code to match expected behavior
3. **Non-determinism**: Check for time/random dependencies

## Extending the Sandbox

### Adding New Mock Adapters

```clojure
(defn my-mock-adapter []
  (reify SomeProtocol
    (method [this arg]
      {:deterministic "response"})))

(swap! (:adapters state) assoc :my-service (my-mock-adapter))
```

### Adding New Golden Transcripts

1. Identify the operation to capture
2. Create sandbox state with appropriate config
3. Execute the operation
4. Serialize to NDJSON
5. Add test in `transport_golden_test.clj`
6. Commit fixtures

## Constraints

The sandbox profile intentionally limits:

- **No network**: All external calls mocked
- **No filesystem writes**: Use atoms/in-memory
- **No randomness**: Seeded or fixed values
- **No real time**: Fixed clock

This ensures transcripts are reproducible across:
- Different machines
- Different times
- Different Clojure versions (within reason)

## See Also

- `test/f2/transport_golden_test.clj` — Test implementation
- `test/fixtures/transport/` — Golden transcript fixtures
- `docs/protocol/golden-transcripts.md` — Protocol-level documentation
- `.github/workflows/test.yml` — CI configuration
