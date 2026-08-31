# C88 — pin the library evidence input

Date: 2026-08-31. Consumer: `library-graph-lint-test/live-library-passes`.

## Split

The authoritative suite formerly called `ensure-live-evidence-index!`, whose complete keyset walk scales with the live evidence corpus. One run passed; the next reached 125 pages and did not terminate in a reasonable gate window. The test now reads `test/fixtures/library-graph/evidence-records.edn`: the 56 records actually cited by `library/aif/attestations.edn`. Its companion pin records SHA-256 `15effe915e59958443e53ce2769723aa6c12edc5b8054386fa26800829a42b81` and the capture basis `{count 197455, max-at 2026-08-31T17:11:16.764732649Z}`.

`scripts/snapshot_library_evidence.bb` is the explicit refresh operation. Refresh is not part of the suite.

The separate bounded live gate, `checks/live_evidence_index_gate.clj`, makes two requests (count and newest record) and emits the live basis. It observed 197,459 records at `2026-08-31T17:11:41.316184051Z`; the four-record increase during this delivery confirms that the source is live.

## Falsifiers and invocations

- `bb -cp . test/library_graph_lint_test.clj` — 4 tests / 16 assertions, exit 0. Existing fixtures reject cycles, dangling targets, unattested edges, body changes, malformed attestations, and evidence semantic mismatches. A fixture-content mutation also fails its pin.
- `bb -cp . checks/live_evidence_index_gate.clj` — exit 0 with a positive count and dated newest record.
- Append `--negative` — exit 0 only because the injected negative count/nil timestamp is rejected; a slipped mutation exits 2.
- `clojure -X:test` — 248 tests / 1,518 assertions, 0 failures/errors, 21 seconds. The earlier 246 count moved under concurrent test additions.
- futon2 `clojure -X:test` — 1,023 tests / 6,155 assertions, 0 failures/errors. The earlier 1,022 count likewise moved by one.

The live corpus size is inventory evidence, not a suite assertion or a completeness claim. The pinned 56-record projection is exactly the evidence consumed by the AIF attestation check; it is not represented as the complete 197,455-record store.
