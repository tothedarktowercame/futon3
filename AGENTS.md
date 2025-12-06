## Maintaining devmap + pattern coherence

- Keep each futon devmap in sync with reality: when a clause is finished, cite the evidence (README section, test, tag); when it is blocked, note the dependency. Use the stack-coherence patterns (e.g. `stack-blocker-detection`) to spot empty dirs or missing artifacts and record `blocked-by[...]` until the prerequisite is done.
- Watch for repeated sigil pairs in devmaps. The pattern library (e.g. `pattern-differentiation-alarms`) treats duplicates as alarms: either split/merge the underlying patterns, add a more specific devmap-coherence flexiarg, or retire stale clauses. After adjustments, rerun the TF–IDF embedding + sigil matrices so the fake embedding stays meaningful.
- Contribute new stack-coherence patterns whenever the project needs cross-futon checks (e.g. comparing READMEs vs git logs). These global patterns should inspect hotspots—git history, README timestamps, file trees—to classify each devmap item as “done / blocked / drifting” and help the informal proof engine keep the whole stack honest.

## MUSN stuff

F2 (MUSN) — One-Page Build Brief

Scope (thin waist): Transport + routing only. F1 (graph/memory) and F3 (ants) are external via adapters.

Non-goals: No storage, no UI, no inference, no arbitrary eval.

Interfaces you must call (adapters supplied elsewhere)
;; F1
(put-event!      f1 run-id event)            ;; -> {:eid ...}
(close-session!  f1 sid)                     ;; -> {:sid ...}
(export-scenario! f1 sid)                    ;; -> {:scenario-path ...}

;; F3
(run-scenario!   f3 scenario-path policy)    ;; -> {:job-id ...}
(job-status      f3 job-id)                  ;; -> {:state :metrics?}

Message protocol (v1) — JSON lines over WS; HTTP POST fallback

hello {client,caps[]} → ack {rev,run-id}

event {msg-id,t,actor,verb,object,prov} → ack {eid,run-id}

session-close {sid} → ack {sid,run-id}

export {sid} → ack {scenario-path,run-id}

run {scenario-path|sid,policy} → ack {job-id,run-id}

status {job-id} → {state,metrics?}

bye → ack

Rules:

Idempotent on msg-id.

200 ms cap on status, 5 s on run submission.

Per-client supervision: errors never crash the server; return {ok:false,err,...}.

Every ack echoes run-id.

Handlers to implement
handle-hello(ctx,msg)         ; mint run-id, negotiate rev
handle-event(ctx,msg)         ; validate → F1.put-event!
handle-session-close(ctx,msg) ; → F1.close-session!
handle-export(ctx,msg)        ; → F1.export-scenario!
handle-run(ctx,msg)           ; resolve sid→scenario if needed → F3.run-scenario!
handle-status(ctx,msg)        ; → F3.job-status

Validation (malli/spec)

Schemas for event, session-close, export, run, status. Hard-fail on unknown fields; include detail in error.

Back-pressure

Queue or drop with {ok:false,err:"overload"} when >1k events/min locally.

Acceptance tests

Replaying 100 identical events (same msg-id) yields the same eids.

export {sid} returns a path; F2 does not touch the file.

run → job-id; status eventually done with metrics.

Killing a client doesn’t affect others; server logs a clean drop.



Repo layout (tiny)
/docs/protocol.md
/src/f2/transport.clj   ; WS/HTTP + timeouts
/src/f2/router.clj      ; decode→validate→dispatch
/src/f2/schemas.edn
/src/f2/adapters/mock.clj
/test/f2/{router_test.clj, transport_test.clj}
/dev/demo.ndjson
/scripts/dev.sh
/Makefile

Make targets
make dev    # start with mock adapters
make demo   # hello → events → session-close → export → run → status
make test   # unit + replay tests

### Emacs HUD tests
`scripts/test-elisp.sh` drives the chatgpt-shell HUD tests. Keep them gentle by supplying
`ERT_SELECTOR` (e.g., `ERT_SELECTOR='futon3-prototype-*'`) and, when running everything, consider
`ulimit -v 2097152` to avoid GUI Emacs OOMs.
