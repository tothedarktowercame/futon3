# Technote: futon3 Tightening Survey

**Date:** 2026-02-08
**Context:** Agency rebuild (M-agency-rebuild.md) in progress. Survey of what else in futon3 could benefit from similar tightening.

## Motivation

Agency wasn't solid partly because it developed in an environment where many things were prototyped simultaneously: FuLab, AIF, independent runners, agent communication, all on top of a repo managing a working catalogue of runners. With the Agency rebuild progressing (A0-A5 invariants, proof tests, Phase 2 implementation), this survey captures what else needs attention.

## Source Tree Status

| Namespace | LOC | Tests | Status | Risk |
|---|---|---|---|---|
| futon3.agency | 2,422 | 7+5 invariant | Active (rebuilding) | Being addressed |
| futon3.musn | 4,952 | 3 | Active | HIGH: other backbone, minimal tests |
| futon3.drawbridge | 1,395 | 0 | Active | MEDIUM: recently hardened, no tests |
| futon3.forum | 776 | 0 | Active | MEDIUM: used for proof trees, no tests |
| futon3.fulab | 1,508 | 2 | Active | LOW: golden-transcript tests are fragile |
| futon3.checks | 10,834 | 1 | Active | LOW: DSL works, stable |
| futon3.chops | 15,494 | 0 | Active | MEDIUM: large, no tests |
| futon3.pattern_hints | 22,141 | 1 | Active | LOW: index-driven, stable |
| futon3.learn_or_act | 12,781 | 1 | Active | MEDIUM: depends on gap_store (abandoned) |
| futon3.aif_bridge | 14,646 | 0 | Active | MEDIUM: large, no tests |
| futon3.tatami{,_schema,_store} | ~20,129 | 1 | Prototype | HIGH: largest untested surface, TODOs, stale |
| futon3.gap_store | 1,019 | 0 | Abandoned | Cleanup candidate |
| futon3.pattern_store | 849 | 0 | Abandoned | Cleanup candidate |
| futon3.futon2_bridge | 1,960 | 0 | Prototype | Cleanup candidate |
| futon3.hx | 1,188 | 3 | Active | LOW: stable |
| f2.transport | 2,022 | 2 | Active | LOW: main orchestrator, tested |

## Scripts & Tooling

**118 scripts** across Bash, Clojure, Python, TypeScript. Key overlaps/issues:

- **IRC bridge**: Clojure (canonical, 12 commits) + Python (shadow/backup) — clarify or remove Python
- **Forum bridges**: 4+ variants (clj, clj-poll, sh, ts) — unclear which are current
- **musn_chat_fucodex.clj**: 1 old commit, superseded by musn_chat_supervisor.clj
- **pattern_index.py, pattern_embedding.py**: ~25 LOC diagnostic scripts, likely superseded by build_pattern_index.clj
- **Hardcoded Linode IP** in fucodex-peripheral.ts (forum WS default)

## Suggested Follow-On Missions (Priority Order)

### 1. MUSN Invariants

MUSN is the other half of the coordination layer. Agency routes messages; MUSN manages sessions, plans, and AIF orchestration. 5k LOC, 3 tests. Needs: session integrity, plan execution atomicity, transport reliability. Same pattern as Agency: spec (flexiarg) -> tests -> fix.

### 2. Dead Code Sweep

Remove or archive: gap_store, pattern_store, futon2_bridge, musn_chat_fucodex.clj, Python IRC bridge (or mark as backup), diagnostic Python scripts. Reduce surface area before adding more features.

### 3. Forum Tests

Even basic integration tests (create thread, post, subscribe via WS, receive) would catch regressions. 776 LOC with no tests is a small investment for coverage.

### 4. Tatami Decision

20k LOC across 3 files, no tests, prototype status since Dec-Jan. Either: (a) commit to it — add tests, integrate with futon1, (b) extract what's useful and archive the rest. Currently the largest untested surface in the repo.

### 5. Multi-Agency Registration

The architecture diagram (`docs/agency.mm`) shows drawbridges connecting to both local and remote Agency instances. Currently Drawbridge only supports a single `agency-ws-url`. Needs: either (a) multi-URL support in Drawbridge so agents register with multiple Agencies simultaneously, or (b) Agency-to-Agency federation so agents register locally and their Agency forwards to peers. The choice affects how standup and bells work across laptop/server boundary. Discovered during Phase 3 validation: Codex drawbridge on laptop only connects to laptop Agency, not Linode Agency, so cross-host standup doesn't work.

### 6. Scripts Consolidation

Document canonical versions of duplicated tooling. Add a tooling matrix to README or a new doc. Remove orphaned scripts.

## Not In Scope

- futon1a rebuild (separate mission: M-futon1a-rebuild.md)
- futon3a stabilization (separate missions: S1-S4)
- futon0-futon7 devmap work beyond futon3 boundary
