# Prototype 1(a) Stack Readiness Report (Futon0-Futon4, optional Futon5)

Scope: Prototype 0–1 clauses for Futon0–Futon4, plus a light Futon5 tail.
Evidence sources: `futon3/holes/futon*.devmap`, repo READMEs, and recent git logs.
Determinism gate: not run (see Futon1 section).

## Futon0
Prototype 0 — System Vitality Indicators
- status[ready]
- evidence[`futon0/README.md` vitality scanner + git activity manifest]
- blocked-by[none]
- mismatch[devmap claims vs README focuses on utilities; README does not name Prototype 0]

Prototype 1 — Epistemic Rhythm & Salients
- status[blocked]
- blocked-by[evidence missing (no concrete artifacts linked)]
- evidence[devmap claims only; no file/fixture link]
- mismatch[README lacks Prototype 1 narrative]

## Futon1
Prototype 0 — Baseline Deterministic Substrate
- status[ready]
- evidence[`futon1/docs/baseline.md`, `futon1/resources/baseline/demo_session.edn`, `futon1/README.md` baseline section]
- blocked-by[none]
- mismatch[none spotted; README asserts prototypes 0–1 are DONE but devmap uses no explicit done status]

Prototype 1 — Demo/Client Determinism & Focus-Header Instrumentation
- status[blocked]
- blocked-by[determinism gate not run in this report]
- evidence[`futon1/README.md` determinism suite + `apps/client` tests]
- drift[commit `b098687` “Prototype1a RC” but devmap has no status update]

Determinism gate (stack-coherence/futon1-determinism)
- status[not-run]
- evidence[`futon1/.github/workflows/futon1-stack-tests.yml` mentioned in README]

## Futon2
Prototype 0 — AIF Stack Baseline
- status[ready]
- evidence[`futon2/doc/stack-baseline.md`, `futon2/doc/trace/single_tick.edn`, `futon2/doc/trace/single_tick.svg`, `futon2/test/resources/goldens/microtraces.edn`]
- blocked-by[none]

Prototype 1 — Observation Layer Hardening
- status[ready]
- evidence[`futon2/test/ants/aif/observe_test.clj`, `futon2/src/ants/aif/observe.clj`]
- drift[commit `84d7fad` “Prototype 1 readiness” but devmap has no status update]

README ⇄ devmap sync
- mismatch[README describes runtime + tests but does not name Prototype 0–1 clauses]

## Futon3
Prototype 0 — Flexiformal Transport Baseline
- status[ready]
- evidence[`futon3/docs/protocol/transport-contract-v1.md`, `futon3/docs/protocol/golden-transcripts.md`, `futon3/docs/sandbox/README.md`, `futon3/test/transport_test.clj`]
- blocked-by[none]

Prototype 1 — Pattern Canon & Standard Library
- status[ready]
- evidence[`futon3/src/futon3/pattern_store.clj`, `futon3/test/pattern_store_test.clj`, `futon3/resources/pattern_store.edn`]
- blocked-by[next-evidence goldens not yet landed]

README ⇄ devmap sync
- mismatch[README covers prototypes 0–5 only; devmap includes prototypes 6–9 with no README mention]

## Futon4
Prototype 0 — Foundations
- status[blocked]
- blocked-by[evidence missing in devmap]
- evidence[README describes baseline setup but no devmap evidence pointers]

Prototype 1 — Arxana Reboot
- status[blocked]
- blocked-by[evidence missing in devmap]
- evidence[`futon4/dev/` + `futon4/test/` exist but not referenced in devmap evidence lines]
- mismatch[README focuses on Prototype 1 client; devmap has no evidence tags]

## Futon5 (optional tail)
Prototype 0 — Nonstarter Frame
- status[blocked]
- blocked-by[design-only plan; no artifacts]
- evidence[`futon5/README.md` notes design intent only]

Prototype 1 — Patterns of Improvisation
- status[blocked]
- blocked-by[no operator specs or protobook artifacts yet]
- evidence[none]

## Staleness Scan (timestamps)
- Futon0 devmap last touched: `futon3/holes/futon0.devmap` (Sat Dec 6 2025)
- Futon1 devmap last touched: `futon3/holes/futon1.devmap` (Fri Dec 12 2025)
- Futon2 devmap last touched: `futon3/holes/futon2.devmap` (Fri Dec 12 2025)
- Futon3 devmap last touched: `futon3/holes/futon3.devmap` (Sun Dec 28 2025)
- Futon4 devmap last touched: `futon3/holes/futon4.devmap` (Tue Dec 2 2025)
- Futon5 devmap last touched: `futon3/holes/futon5.devmap` (Sat Dec 6 2025)
- README touchpoints: Futon0 (Dec 26), Futon1 (Dec 13), Futon2 (Dec 28), Futon3 (Dec 28), Futon4 (Dec 27)

## Prototype 1(a) Exit Criteria Check
- Every clause in Futon0–Futon4 has a status + evidence or named blocker: partial (Futon4 missing evidence for P0–P1).
- Futon1 determinism gate: not run, blocks Prototype 1(a) completion.
- README ↔ devmap alignment: mismatches in Futon0, Futon2, Futon3, Futon4.
