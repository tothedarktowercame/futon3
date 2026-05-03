# Follow-up Checkpoint — substrate-2 / M-live-geometric-stack

**Date:** 2026-04-29
**Owner:** Joe
**Mission:** `futon3/holes/missions/M-live-geometric-stack.md`
**Scope of this follow-up:** close the phase-4.6 gap around same-root and
cross-root moves; use the live War Machine consolidation as the proving
ground; establish operator-facing test futons and harnesses.

---

## What landed

### 1. Phase-4.6 cascade semantics now exist

- `futon3/scripts/multi_watcher.clj` now:
  - re-ingests rename destinations,
  - marks vacated vertices with `:edge/witness-stale true`,
  - emits deterministic `edge/renamed-to` links when old/new pairing is
    unambiguous,
  - supports bounded runs via `--max-cycles`,
  - detects unique cross-root moves (`futonX -> futonY`) by content hash and
    applies the same stale/link cascade across watched roots.

### 2. War Machine consolidation into futon2 landed

- Canonical War Machine engine + visualiser moved into `futon2`.
- Runtime callers were repointed:
  - `futon2/web/war-machine`
  - `futon3c` `/api/alpha/war-machine`
  - `futon0/emacs/war-machine.el`
- The move was then cleaned up in the live substrate so old `futon0`
  file-owned War Machine vertices are stale and the new `futon2` ones are
  live.

### 3. Regression coverage exists for same-root and cross-root moves

- Same-root rename/delete regression:
  - `tests/phase_4_6_cascade_test.clj`
- Cross-root move regression:
  - `tests/phase_4_6_cross_root_move_test.clj`
- The cross-root regression covers:
  - watcher event observed,
  - cascade observed,
  - old-side vertices stale,
  - new-side vertices live,
  - directed `edge/renamed-to` old -> new,
  - conservative abstention on ambiguous same-hash batches.

### 4. Operator-facing test futons now exist

- Real watched roots created for manual experiments:
  - `~/code/futonX`
  - `~/code/futonY`
- Harness script:
  - `futon3/scripts/test_futons_cross_root_move.clj`
- Harness modes:
  - `--mode single`
  - `--mode batch`
  - `--mode ambiguous`

---

## What was verified live

- The War Machine move cleanup succeeded in the substrate.
  Old `futon0` file-owned vertices for the moved War Machine files are stale;
  new `futon2` vertices are live.
- `tests/phase_4_6_cross_root_move_test.clj` passes against the live store.
- Early real-root harness runs on `futonX`/`futonY` proved the expected shape
  for single-file cross-root moves and wrote reports under `/tmp/`.

Relevant artifacts:

- `/tmp/war-machine-after-cleanup.edn`
- `/tmp/test-futons-cross-root-1777452419795.edn`
- `/tmp/test-futons-cross-root-1777453419698.edn`

---

## Current blocker

### B-4 — watcher source-file lookups still hit the whole store unless futon1a is reloaded

The operator harness exposed a real performance issue:

- `multi_watcher.clj` was fetching all `code/v05/{namespace,var,test}`
  hyperedges for the whole store and filtering client-side by `:repo` and
  `:source-file`.
- On the live substrate this makes `batch` and `ambiguous` harness runs stall
  while waiting for the watcher to finish.

Clean fix already coded:

- `futon1a/src/futon1a/api/routes.clj`
  - `GET /api/alpha/hyperedges?type=...` now accepts optional `repo` and
    `source-file` filters.
- `futon1a/src/futon1a/http/app.clj`
  - passes `repo` and `source-file` through to the route handler.
- `futon3/scripts/multi_watcher.clj`
  - now queries `type + repo + source-file` directly.

Code compiles, but the running futon1a on `localhost:7071` still has the old
route behavior until it is reloaded or restarted by the proper orchestrated
path.

Important discipline note:

- `futon1a/CLAUDE.md` says **never kill or restart the running futon1a
  server** from an ad hoc session. So the live proof of the faster harness is
  deferred until futon1a is refreshed safely.

---

## Immediate next step when returning to this thread

1. Refresh futon1a through its orchestrated path so `:7071` serves the new
   `repo` / `source-file` query filters.
2. Re-run:
   - `bb /home/joe/code/futon3/scripts/test_futons_cross_root_move.clj --mode batch --count 2 --max-cycles 8`
   - `bb /home/joe/code/futon3/scripts/test_futons_cross_root_move.clj --mode ambiguous --count 2 --max-cycles 8`
3. Confirm:
   - `batch` completes and emits 2 cross-root move + 2 cascade events,
   - `ambiguous` completes and conservatively abstains from auto-pairing,
   - `~/code/futonX` and `~/code/futonY` clean up correctly after non-`--keep`
     runs.

---

## Status line

Phase 4.6 semantics are structurally in place and the War Machine move was a
successful substrate-level test. The remaining gap is operational: live
verification of the heavier harness modes is waiting on a safe futon1a refresh
so the new server-side hyperedge filters are actually in service.
