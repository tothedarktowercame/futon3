# Excursion: Live Means Live — closing substrate-2's commit-coverage freshness gap

**Date opened:** 2026-05-01
**Status:** open
**Owner mission:** `futon3/holes/missions/M-live-geometric-stack.md` (parent — this excursion holds the follow-up to the 2026-05-01 Checkpoint there)
**Sister missions:**
- `futon3c/holes/missions/M-reachable-from-boot.md` (the anti-pattern this issue inherits is the same family that mission opened)
- `futon5a/holes/missions/M-war-machine-wiring.md` § VERIFY Checkpoint 1 (the discovery context — substrate-2's commit lag was surfaced while running a stepper trial for the wiring mission)

**Audience:** Joe (operator), substrate-2 maintainer

## The gap, in one sentence

`M-live-geometric-stack` ships var/test/namespace coverage as **real-time** (multi-watcher, ~5s polling) and commit coverage as **manual-batch** (operator runs `bb scripts/ingest_commits_to_futon1a.clj` per repo). The "live" predicate the mission's name implies is therefore partial: it holds for one vertex class and not for another.

## Evidence

Sampled 2026-05-01 19:00 UTC:

| repo | latest-indexed commit | days behind today |
|---|---|---|
| futon0-d | 2026-04-25 | 6 |
| futon1a-d | 2026-04-12 | 19 |
| futon2-d | 2026-04-27 | 4 |
| futon3-d | 2026-04-27 | 4 |
| futon3c-d | 2026-04-25 | 6 |
| futon5a-d | 2026-04-17 | 14 |
| futon6-py-d | 2026-04-27 | 4 |

A 5-commit trajectory in futon3c on 2026-05-01 (M-invariant-queue-unstuck → M-reachable-from-boot hot-fix) is invisible to substrate-2 even though every file the commits touched is being watched by the multi-watcher in real time.

## The structural diagnosis

Same anti-pattern as `M-reachable-from-boot` (`futon3c/holes/missions/M-reachable-from-boot.md`) — long-lived state populated only at operator-driven activation time. That mission's family `layered-error-hierarchy/reachable-from-boot/<container>` watchdogs the JVM-internal cases (evidence-store, agent-registry, dev-evidence-store, family-check-fns). Substrate-2's commit-vertex ingestion is a cross-process sibling that wasn't in that mission's INSTANTIATE scope.

The "loaded guns on Chekhov's desk" framing applies directly: nothing structurally prevents the freshness from drifting arbitrarily far from the actual git state. The script exists; running it is operator-discipline-bound.

## Options considered

### (1) Cron-scheduled ingestion — REJECTED

Run `bb scripts/ingest_commits_to_futon1a.clj <repo-root> --label <tag>` on a timer (e.g. every 15 minutes per repo).

**Rejected per Joe 2026-05-01.** This is exactly the
operational-discipline-as-substitute-for-structure hot-fix that the
M-reachable-from-boot family was opened to prevent. The "loaded gun"
remains: a cron job that dies silently, or that the operator forgets
to restart after a server reboot, produces the same drift the
manual approach does. Cron papers over the structural issue rather
than closing it.

The script remains useful as a **backfill tool** (initial population
of a new repo, or recovery after some outage) but should not be
deployed as the everyday ingestion path.

### (2) Watcher-integrated commit ingestion — STRUCTURAL FIX

The same multi-watcher process that handles file events watches the
git refs per repo (likely `.git/refs/heads/` plus `.git/HEAD`) and
triggers commit-vertex emission whenever new commits appear.
Closes the freshness gap to seconds and removes the two-cadence
asymmetry: the watcher becomes the single source of liveness across
all vertex classes.

### (3) Invariant sibling under reachable-from-boot — STRUCTURAL PROTECTION

A new sibling under `reachable-from-boot/<container>`:
`reachable-from-boot/substrate-2-commit-coverage`. Definition: for
every watched repo, `(now - latest-indexed-commit-timestamp(repo))
≤ N seconds`. Default N: 60 seconds (with watcher-integrated
ingestion landed). Probe runs hourly + on-demand + autoshutter,
matching the I-family-canary pattern from M-invariant-queue-unstuck
(commit 557efe4).

This is what makes (2) **structurally protected** rather than
trust-based: even if the watcher's commit-detection path silently
breaks in the future, the freshness invariant fires within an hour
and surfaces the regression on the WM display.

## Solution sketch: (2) + (3) together

(2) closes the gap; (3) prevents it from re-opening silently. They
land together as the closure path for `E-live-means-live`.

### Architecture for (2): watcher-integrated commit ingestion

Three components, in the order they should be built:

#### Component A: shared ingestion library

The existing `futon3/scripts/ingest_commits_to_futon1a.clj` is a
top-to-bottom `bb` script with an implicit `--main`. Refactor into a
loadable library, e.g. `futon3/scripts/commit_ingest_lib.clj`,
exposing pure functions:

```clojure
(ns commit-ingest-lib)

(defn last-indexed-commit-sha
  "Returns the SHA of the most-recently-ingested commit for repo, or
   nil if no commits indexed yet. Queries futon1a directly — no
   caller-side state."
  [repo-label]
  ...)

(defn new-commits-since
  "Returns a vector of new commit SHAs in chronological order (oldest
   first) for repo since the given SHA. If since-sha is nil, returns
   the full history."
  [repo-root since-sha]
  ;; uses `git rev-list <since-sha>..HEAD --reverse` (or full reverse log)
  ...)

(defn ingest-commit!
  "Idempotent. Emits the four hyperedge types (commit, author,
   authored, precedes, edits) for one commit. Re-running on a
   already-indexed commit is a no-op."
  [repo-root repo-label sha]
  ...)

(defn ingest-new-commits!
  "Compose: query last-indexed, list new commits since, ingest each.
   Returns a small report map {:repo :n-ingested :latest-sha}."
  [repo-root repo-label]
  ...)
```

Both the existing standalone script and the watcher integration call
into this library. Single-source-of-truth for ingest logic.

#### Component B: watcher loop integration

The multi-watcher (`futon3/scripts/multi_watcher.clj`) currently
polls filesystems at ~5s intervals. Extend its per-cycle work to
also call `commit-ingest-lib/ingest-new-commits!` per repo:

```clojure
;; Inside the watcher's per-cycle loop:
(doseq [{:keys [repo-root repo-label] :as repo} watched-repos]
  ;; existing: handle file changes, emit var/test/namespace hyperedges
  (handle-fs-changes! repo)
  ;; new: pick up any new commits
  (let [report (commit-ingest-lib/ingest-new-commits! repo-root repo-label)]
    (when (pos? (:n-ingested report))
      (log/info :commit-ingest report))))
```

Detection strategy (recommended): **D3 — `git rev-list` diff per
cycle.** The watcher already runs every ~5s; calling `git rev-list
<last-indexed>..HEAD --reverse` per repo per cycle is cheap (one git
invocation, returns nothing if no new commits) and matches the
existing polling model. Inotify on `.git/refs/heads/` would be more
elegant but adds platform-specific code; D3 stays portable.

#### Component C: state-of-last-indexed lives in substrate-2 itself

Critical M-reachable-from-boot lesson: **don't store
last-indexed-sha in a separate file or atom.** Query it from
substrate-2 directly: *"what's the SHA of the most recent
`code/v05/commit` vertex for this repo?"* This makes the watcher
restartable — kill it, bring it up, and it picks up from wherever
the substrate is, with no missed commits and no double-ingestion.
This is the construction-path-discipline principle applied here.

### Architecture for (3): the invariant sibling

Lives in futon3c alongside the four existing siblings, file
`futon3c/src/futon3c/logic/reachable_from_boot.clj` (or wherever
the family's existing siblings are housed — verify in
`structural-law-inventory.sexp` line ~200 area).

XTDB's bitemporal model does the heavy lifting: substrate-2 already
records each commit-vertex with a `valid-time`, so "what does
substrate-2 know as of now?" is a native query, not state we have
to track. The probe is just a comparison:

```clojure
(defn check-substrate-2-commit-coverage
  "Returns :ok | :violation | :inactive.
   For each watched repo: query substrate-2 for its latest commit
   timestamp (XTDB :as-of (now)); query git for HEAD's timestamp;
   compare. Tolerance N exists only because watcher cycles take a
   few seconds — sustained lag past N means something's wrong."
  [{:keys [now n-seconds]}]
  ...)
```

Probe schedule mirrors I-family-canary: hourly + on-demand +
autoshutter. Fires `:family-fired :substrate-2-commit-coverage
:ok|:violation|:inactive` evidence per cycle.

The invariant gets registered the same way the existing siblings do
— via `register-default-taps!` or similar — and shows up on the WM
display next to the other reachable-from-boot heartbeats.

### Verifications

These are conditions the (2)+(3) closure must satisfy. Each is
checkable against running infrastructure, not operator judgment.

**V1 — idempotency.** Ingest the same commit twice via the shared
library; the substrate state must be byte-identical (or at least
semantically identical — same hyperedge IDs, same props, no
duplicate edges). *Failure mode caught: a v0 implementation that
re-emits edges and accidentally creates duplicate `:authored`/
`:precedes` records.*

**V2 — catch-up latency.** Commit to a watched repo at time T;
query substrate-2 for the commit's vertex at time T+5s; the vertex
must be present and complete. *Failure mode: the watcher detects
the change but ingestion fails silently, leaving the commit
invisible despite the watcher running.*

**V3 — watcher-restart resumability.** Kill the watcher process;
commit N times to a watched repo while it's down; bring the watcher
back; verify all N commits are ingested within one cycle of restart.
*This is the M-reachable-from-boot-shape verification — proves
last-indexed-state lives in substrate-2 itself, not in process
memory.*

**V4 — multi-repo concurrency.** Commit simultaneously to two
watched repos; verify both are ingested correctly with no state
crossing between them. *Failure mode: shared state in the
ingest-lib that assumes single-repo invocation.*

**V5 — freshness invariant active.** With (3) deployed, force a
synthetic stale state (e.g. set substrate-2's latest-indexed
timestamp for one repo to two minutes ago, or stop the watcher for
two minutes); verify the probe fires `:family-fired
:substrate-2-commit-coverage :violation` within one probe cycle.
*Failure mode: the invariant exists but never fires because the
threshold is wrong or the probe's repo-set is incomplete.*

**V6 — backfill tool still works.** The standalone script
`ingest_commits_to_futon1a.clj` continues to function for backfill
scenarios (new repo onboarding, recovery after substrate-2 reset).
After running the script on a fresh repo and bringing the watcher
up, the watcher must hand off correctly without re-ingesting or
missing commits.

### Implementation order

I0. **Refactor: shared library.** Extract `commit_ingest_lib.clj`
    from the existing script. The standalone script becomes a thin
    wrapper around the library. **No behaviour change yet** — same
    output, same idempotency. Test V1.

I1. **Watcher integration.** Add the per-cycle `ingest-new-commits!`
    call to `multi_watcher.clj`. Test V2, V3, V4. At this point
    "live means live" is operationally satisfied.

I2. **Invariant sibling.** Implement and register
    `reachable-from-boot/substrate-2-commit-coverage`. Test V5.
    Now "live means live" is **structurally enforced**.

I3. **Documentation.** Update `M-live-geometric-stack.md`'s
    Checkpoint to mark E-live-means-live as CLOSED. Update this
    excursion's status. The standalone script's role is reduced to
    backfill — note this in its preamble (V6).

### Open design questions

These are real questions the implementation will surface and that
should be settled before or during build, not papered over:

1. **Force-pushes / rebases.** A commit that was indexed but no
   longer exists in any branch's history (e.g. the operator
   force-pushed to drop it). Should substrate-2 retain the commit
   vertex (historical record) or retire it (current-state record)?
   The existing script's idempotency-preserves-history would
   silently keep retired commits. *Recommendation*: retain by
   default with a `:retired? true` prop set when no current branch
   reaches the commit; periodic sweep updates the flag. Downstream
   queries can filter on `:retired? false` when they want
   current-state.

2. **Initial onboarding of a new repo.** When a new futon enters
   the watcher's coverage list, does the watcher backfill all
   commits, or only watch from "now"? *Recommendation*: explicit
   backfill via the standalone script as part of onboarding, then
   handoff to live ingestion. Trying to backfill from inside the
   watcher's per-cycle loop risks long blocking calls on first
   cycle.

3. **`:edits` edge resolution.** The existing script resolves
   `:edits` against the var set at HEAD's snapshot — a v0
   approximation per its preamble PSR. Watcher-integrated
   ingestion has access to the full file change history per
   commit. *Recommendation*: keep the v0 approximation in the
   shared library (matches existing behaviour); raise the v1
   precision question as a separate finding section in this
   excursion if it ever bites.

4. **Threshold N for the freshness invariant.** Default 60s.
   Adjust if observation shows it wrong. XTDB's bitemporal model
   carries the freshness reasoning natively; N is just a tolerance
   that tells the probe "this much transient lag is expected."
   No multi-week tuning ceremony required — it's a number to
   change once if the default is bad.

5. **Don't cache last-indexed-sha in process memory.** The
   watcher should query substrate-2 each cycle for its view of
   "latest commit per repo." XTDB's queries are cheap; this
   keeps the construction-path-discipline principle clean (the
   substrate is the only source of truth) and avoids the
   "stale atom" anti-pattern that M-reachable-from-boot was
   opened to prevent.

## Audit task: per-vertex-class liveness

Independently of closing this specific gap, this excursion proposes an audit: for every vertex class substrate-2 indexes (`:var`, `:test`, `:namespace`, `:term`, `:pattern`, `:commit`, `:author`, `:doc`, `:mission`, `:evidence`, `:claim`, `:proof`), document the freshness mechanism and verify the "live" predicate. Some classes may be implicitly live via the watcher; others (like `:commit`) may have hidden manual-batch dependencies. The audit becomes another finding section under this excursion as it accretes.

## Discovery context

The freshness gap was surfaced while running M-war-machine-wiring's VERIFY V-2.4 stepper trial (`futon5a/holes/missions/M-war-machine-wiring.md`, Checkpoint 1, 2026-05-01) against a 5-commit M-reachable-from-boot trajectory in futon3c. The trial expected to read `(vertex, commit)` pairs at each step's HEAD~k position; substrate-2 returned no commit-vertices for those commits because they were post the latest manual ingestion. The wiring mission can't proceed mechanically against fresh fixes until either (1) or (2) above lands.

## Findings (accretes here)

### §1. I0 + I1 v0 deployed against futon3c-d (2026-05-01)

I0 (shared library `commit_ingest_lib.clj` extracted from the
standalone backfill script) and I1 v0 (multi-watcher integration
calling `ingest-new-commits!` per cycle per root) were built and
exercised in a 1-cycle bb instance (`bb scripts/multi_watcher.clj
--root /home/joe/code/futon3c=futon3c-d --max-cycles 1
--no-cold-scan`). Results:

| | before | after | delta |
|---|---|---|---|
| commits | 598 | 604 | +6 ✓ |
| authors | 1 | 2 | +1 ✓ |
| authored | 598 | 604 | +6 ✓ |
| precedes | 597 | 603 | +6 ✓ |
| edits | 75789 | 75789 | **0** (see §2) |

The 5 commits of the M-reachable-from-boot trajectory (557efe4,
bac7422, eb02d6d, 314ca64, 18b5336) are now indexed.
`last-indexed-commit-sha("futon3c-d")` returns 18b5336 (current HEAD).
The V-2.4 stepper-trial blocker for that trajectory is closed.

The running multi-watcher process (bb pid 1747164) has NOT yet been
restarted; it's still on pre-I1 code. Restart will pick up I1 across
all 12 watched repos with the largest catch-up burst happening on
the first post-restart cycle.

### §2. Limitation: `:edits` skipped when var-vertex `:source-file` is missing

Substrate-2's `:code/v05/var` and `:code/v05/test` vertices
ingested before the B-4 follow-up don't carry a `:source-file`
prop. Sampled futon3c-d's 1.7MB var-vertex dump: only 1 occurrence
of `source-file` across thousands of records. Same pattern in
namespace-vertices.

I1's `query-repo-vars-by-file` builds the `(file → vars)` map by
reading `:source-file` from var-vertex props. When that prop is
empty, the map is empty, and `ingest-edits-for-commit!` emits 0
`:edits` per commit. That's why the 6 newly-ingested futon3c-d
commits added 0 `:edits` to the 75789 pre-existing ones.

**Implications:**

- Live-mode commits get correct commit/author/authored/precedes
  structure, but no `:edits` to vars they touched, until
  substrate-2's `:source-file` coverage is uniform.
- The gap closes **naturally over time** as the running watcher
  re-emits var-vertices on file changes — newer watcher-emitted
  vars do carry `:source-file`. The `:source-file` coverage
  improves whenever a file is edited and re-ingested.
- Standalone backfill via `ingest_commits_to_futon1a.clj`
  remains the path that *does* emit `:edits` correctly (it
  computes file→vars by walking the filesystem, not by querying
  substrate-2).
- A follow-up backfill of `:source-file` onto pre-B-4
  var-vertices would close the gap explicitly rather than
  waiting for natural attrition. Logged as a candidate task.

### §3. Limitation: format-level qname coexistence in `:edits`

Independently of §2, substrate-2 has two formats of `:edits` qnames
coexisting:

- **Pre-B-2-fix**: unprefixed —
  `futon3c.peripheral.mission-control-backend/attach-doc-audit`
- **Post-B-2-fix** (current convention): per-repo-prefixed —
  `futon3c-d/futon3c.peripheral.mission-control-backend/attach-doc-audit`

futon3c-d's existing 75789 `:edits` use the unprefixed format.
futon7-d (which the standalone backfill ran against during I0)
ended up with 318 `:edits` where each commit-var relationship is
represented *twice*: once unprefixed (pre-existing) and once
prefixed (added by I0).

**Implications:**

- Queries traversing `:edits` may need to handle both formats, or
  pick a normalization rule per use case.
- A substrate-2-wide cleanup that retires the unprefixed form
  would be a real piece of work (75789 records on futon3c-d
  alone). Out of scope for E-live-means-live; flagged as a
  separate substrate-2 issue worth its own excursion or mission.
- For now, do not assume `:edits` qname format is uniform when
  reading from substrate-2.

### §4. Implication: V0 (data validity) is a missing verification class

The I0 build hit a U+001F separator bug that produced 71 corrupt
records (1-character SHAs) before being detected. Critically, the
buggy first run completed *cleanly* with `n-failed: 0` reported —
the library's failure detection only checked HTTP status, not data
validity. Idempotency (V1) doesn't help here: it only guarantees
that whatever-was-posted gets posted-the-same-way on re-run; it
doesn't guarantee what-was-posted was correct in the first place.

**Implications:**

- The verification list (V1–V6) needs a new class: **V0 — data
  validity**. Pre-flight checks that the data about to be posted
  is well-formed: SHAs match `[0-9a-f]{40}`, emails contain `@`,
  timestamps are positive integers in a reasonable range, etc.
  V0 fires *before* the POST, not after.
- This applies to both the standalone script and the watcher
  integration. Both call into `ingest-commit-and-authored!` and
  similar; V0 belongs in the library at the entry to those
  primitives.
- Without V0, idempotency is a hollow guarantee: a buggy
  ingestion path corrupts substrate-2 in ways that are
  hard to clean (no public DELETE; required Drawbridge nREPL
  surgery this round).
- M-war-machine-wiring's V-1 should also pick up this finding
  (V1 alone doesn't certify correctness; pair it with V0).

### §5. Implications for V-2.4 mechanical mode

The M-reachable-from-boot trajectory was the original probe that
surfaced the live-means-live gap. With those commits now indexed
in substrate-2:

- V-2.4 mechanical mode (T at `(vertex, commit)` pre/post a fix)
  is now runnable for the trajectory. The earlier stepper trial
  proceeded by-hand on prose evidence; a re-run can now use real
  substrate-2 quantities at HEAD~5 through HEAD.
- The `:edits` gap (§2) means trajectory queries that traverse
  `:edits` (e.g. "which vars did commit X touch?") will return
  empty or partial answers. T/∇T/ΔT/drift queries do *not*
  depend on `:edits` and remain accurate.
- The V-2.4 vocabulary trial recorded in M-war-machine-wiring
  Checkpoint 1 should be re-run once the running watcher
  restart lands; the by-hand readings can then be cross-checked
  against auto-generated ones for vocabulary refinement.

### §6. Architectural correction: watcher must run in-JVM (Joe 2026-05-01)

The current state has TWO JVMs: futon3c (pid 1747233, hosting the
Drawbridge nREPL + futon1a HTTP) and the multi-watcher (pid 1747164,
running `multi_watcher_jvm.clj`). The intended architecture is **one
JVM** — the watcher should be a service inside futon3c, not a
separate process.

This is structural, not aesthetic: the machine isn't sized for two
JVMs. The current two-JVM state is a transitional artefact (the
watcher started bb-only, migrated to JVM-with-WatchService, and the
integration into the main futon3c JVM was never completed).

The deviation also bites this E-live-means-live excursion directly:
the watcher process has no Drawbridge nREPL, so I1 deployment
requires a process restart rather than a code-reload. Migrating to
in-JVM removes that constraint — future watcher updates deploy via
the same mechanism as everything else in futon3c.

#### Migration plan (Path B)

**Module layout.** Port the bb scripts into futon3c source (futon3c
CLAUDE.md I-5 forbids depending on `/home/joe/code/futon3/scripts/`):

```
futon3c/src/futon3c/watcher/
  multi.clj                — main loop (was multi_watcher.clj)
  commit_ingest.clj        — commit-vertex ingestion (was
                             commit_ingest_lib.clj)
  file_ingest.clj          — per-file vertex ingestion (was
                             ingest_one_file.clj)
  projections/
    elisp.clj              — was elisp_projection.clj
    python.clj             — was python_projection.clj
```

**Refactor approach.** The bb-isms map cleanly to JVM Clojure:

- `(load-file …)` → `(:require …)` in ns declarations.
- `babashka.fs`, `babashka.http-client`, `clojure.java.shell`,
  `cheshire.core` are all available on the JVM with the same APIs;
  just need to be on the deps.edn classpath (most already are).
- `(when (= *file* (System/getProperty "babashka.file")) (apply
  -main *command-line-args*))` → drop, replace with explicit
  `start!` invoked from bootstrap.clj.

**Service shape.** Match the existing pattern from
`futon3c/src/futon3c/portfolio_inference/scheduler.clj` (per the AIF
tick mission):

- `defonce` atom holding the running future (or nil if stopped).
- `start!` / `stop!` / `status` / `tick!` (manual one-shot) /
  `set-period!` public functions.
- Register with the futon3c `cyder` process surface as
  `:type :daemon`, `:layer :repl`.
- Single-thread loop via `ScheduledExecutorService` with
  `scheduleWithFixedDelay`, so slow cycles don't pile up.
- Per-cycle try/catch isolates errors from the loop.

**Bootstrap integration.** In
`futon3c/dev/futon3c/dev/bootstrap.clj`'s `run-main!`, after
`start-futon1a!` lands (so substrate-2 is queryable), call
`(futon3c.watcher.multi/start! {…opts…})`. Opts include the same
`:roots`, `:interval-ms`, `:watch-mode`, `:debounce-ms` the current
script accepts — just passed as data instead of CLI args.

**Reload-safety considerations** (per `feedback_reload_safety`):

- No protocol-defining namespaces in this port. The watcher uses
  plain functions; protocols add nothing here.
- The per-root snapshot atom keeps state across reloads (good —
  reload doesn't lose the cold-scan baseline).
- The loop's body should call helper functions through their vars
  (never close over them at scheduler-creation time) so a code
  reload picks up new logic on next cycle.
- Bootstrap replay is satisfied by `cold-scan?` — on JVM restart,
  the watcher rebuilds the snapshot cache from disk on first cycle.

**Cutover plan.** Three steps in sequence, each verifiable:

1. **Port code**, write tests, add to deps.edn classpath. No
   running-state change yet; new namespaces just exist.
2. **Eval-test in Drawbridge** — load the new namespace, call
   `start!` *with a one-repo restricted opts map* (e.g. just
   futon7), verify it produces the same hyperedges as the
   separate-process watcher would. This runs the in-JVM watcher
   alongside the separate-process one for a brief window — no
   conflict because hyperedges are idempotent.
3. **Cutover**: kill the separate watcher process (pid 1747164),
   then `start!` the in-JVM watcher with the full 12-root opts.
   Update `dev-laptop-env`'s `start_multi_watcher` to no-op (or
   only kill any orphans, no spawn).

**Per-file ingest path.** The current watcher shells out to
`bb scripts/ingest_one_file.clj` per file change (line 46 in
multi_watcher.clj defines `INGEST-SCRIPT`, used by the `ingest-event!`
function). In one-JVM, this becomes a direct function call to
`futon3c.watcher.file-ingest/ingest-file!`, no subprocess. Big
performance win on top of the architectural alignment.

**Verification additions on top of V1–V6.** Migration introduces
new failure modes:

- **V7 — port-equivalence**: for one repo run via the in-JVM
  watcher, the substrate-2 effects (commit/author/authored/
  precedes/edits + the per-file var-vertex updates) must be
  identical to what the separate-process watcher would produce.
  Test by running both watchers against the same repo for one
  cycle (the idempotency property makes this safe) and diffing
  hyperedge counts + sampled records.
- **V8 — reload-survival**: with the in-JVM watcher running,
  redefine one of its helper functions via Drawbridge eval and
  verify the next cycle picks up the change without restart.
- **V9 — bootstrap-replay**: kill the futon3c JVM, restart it,
  verify the watcher comes up clean and resumes ingestion within
  one cycle of futon1a being available.

#### Implications back to E-live-means-live's earlier sections

§1's deployment of I1 v0 was done in the **two-JVM state** —
specifically, a 1-cycle bb instance run against futon3c-d. That
catch-up's effects (6 new commits ingested) are valid; what they
*don't* tell us is whether I1 works inside a long-running in-JVM
watcher service. V7 above is the test that confirms the port
preserves the I1 semantics.

#### Recommended next steps (after this plan is approved)

1. **Port code (Path B step 1)** — module layout above, no
   running-state change yet.
2. **Write V0 (data validity) checks** into `commit_ingest.clj`'s
   primitives at port time, since we're touching that code anyway.
3. **Eval-test (Path B step 2)** — Drawbridge-load the new
   namespace, run V7 against futon7 (smallest repo).
4. **Cutover (Path B step 3)** — kill separate watcher, start
   in-JVM watcher, run V8 + V9.
5. **Backfill `:source-file` onto pre-B-4 var-vertices** as a
   separate focused task (§2's gap closure).
6. **Open a separate excursion or mission** for §3's qname format
   cleanup.
7. **Land (3) — the `reachable-from-boot/substrate-2-commit-coverage`
   invariant sibling** in futon3c. With the in-JVM watcher
   running, the invariant lives in the same process as the
   thing it watchdogs.
