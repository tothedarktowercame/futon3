# Excursion: Pattern Peripheral (transactional pattern creation)

**Status:** OPEN — Codex handoff, IDENTIFY (2026-05-04).
**Owner mission:** `futon3/holes/missions/M-pattern-retrieval-calibration.md`
**Companion to:** P-1 (canonical flexiarg parser, futon3a) — the parser is
this peripheral's validation gate.
**Cross-refs:**
- `futon0/holes/missions/M-the-futon-stack.md` §"Cross-cutting threads" /
  CCT-1 — the canonical-parser invariant. The peripheral is how
  pattern-creation honours CCT-1 at write time.
- `futon3/holes/missions/M-pattern-application-diagnostic.md` — typed
  slot schema; once a pattern is being lifted there, the peripheral
  enforces typed-slot completeness as part of validation.
- `futon3c/holes/missions/M-autonomous-pattern-lifecycle.md` (parked) —
  covers *select / use / assess* (PSR/PUR/PAR). This excursion is the
  *create* verb that mission has no story for.
- `futon3c/CLAUDE.md` — peripheral spec; gate pipeline (G5..G0) is the
  transactional shape this peripheral consumes.

## Why this is needed

Agents have been creating `.flexiarg` files with approximately-arbitrary
clause structure. Example:
`futon3/library/structure/block-as-futonic-revolution.flexiarg` — a
rich, well-authored pattern, but with bespoke clauses
(`+ APPLICATION-TO-WORKING-TREE:`, `+ APPLICATION-TO-MISSIONS:`,
`+ APPLICATION-TO-AGENT-EVIDENCE:`, `+ DIAGNOSTIC:`,
`+ ANTI-PATTERNS:`, `+ COMPOSITION-WITH-SIBLINGS:`) added alongside the
canonical `+ context: / + IF: / + HOWEVER: / + THEN: / + BECAUSE:`
vocabulary.

Two consequences:

1. **Retrieval substrate fragments.** Two patterns about similar work
   may use different clause names. Embedding catchment widens; HIT
   rationale (M-pattern-retrieval-calibration §M-4) loses the
   `BECAUSE`-overlap signal because the operator wrote `RATIONALE:`
   instead of `BECAUSE:`.
2. **Typed-slot lift becomes a moving target.**
   M-pattern-application-diagnostic's `{:context :tension :move
   :witness-shape :domain}` schema has to map onto whatever clauses
   the author chose, individually for every pattern. The schema-side
   work and the authoring-side work drift independently.

The fix is **transactional pattern creation** — pattern files don't
land via free-form file writes; they land through a peripheral that
parses, validates, and emits evidence on landing. The peripheral is
the symmetric *creation* half to the *retrieval* half
M-pattern-retrieval-calibration is building.

## What "transactional" means here

A **pattern-creation transaction** is one block (per
`library/structure/block-as-futonic-revolution.flexiarg`):

- input: `{author, target-path, content, mode}` where mode is
  `:advisory` (default) or `:enforced`
- gate sequence (futon3c gate pipeline, scoped):
  - **G5 — validated proposal**: target path exists under a known
    library root; content is non-empty; author is a registered agent.
  - **G4 — parser-valid**: the canonical parser
    (`futon3a/src/futon/flexiarg/projection.clj` per
    M-pattern-retrieval-calibration §D-10) returns a packet without
    error.
  - **G3 — vocabulary check**: each clause name in
    `:pattern/clauses[*].name-key` is in the canonical vocabulary
    allowlist; sigils resolve in the existing
    `scripts/sigil_allowlist.clj` allowlist; namespace
    (`@flexiarg <ns>/<name>`) matches the filesystem path.
  - **G2 — references resolve**: every `@references [...]` entry
    either resolves to an existing pattern packet or is recorded as
    a forward-reference with `:reference/state :unresolved`.
  - **G1 — atomic write**: file is written; canonical packet is
    appended to / replaces its row in `pattern-projections.edn`;
    embeddings are queued for re-embed.
  - **G0 — durable evidence**: emit `event "pattern-created"` with
    `{author, path, qname, parse-status, vocab-status, refs-status,
     mode, evidence-id}`.
- output: `{:landed? true, :evidence-id ...}` or
  `{:landed? false, :violations [...]}`.

Outcome: either *all* of G5..G0 succeed and the pattern lands, or
*none* of the durable side-effects happen (no file write, no index
update, no embedding queue). The evidence record is emitted in both
cases — successful and refused transactions are both observable.

## Modes

- **`:advisory`** (phase-1 default, ships first): every gate runs;
  violations at G3 / G2 are logged as
  `event "pattern-create-violation"` evidence with the violation
  details, but the transaction still proceeds to G1 / G0 if G4
  parser-validity holds. The substrate accumulates a violation
  corpus that informs vocabulary curation.
- **`:enforced`** (phase-2, behind a feature toggle): G3 / G2
  violations refuse the transaction. Operator can override per-call
  with `:mode :advisory` if a legitimate exception exists; override
  is itself recorded as evidence.

Phase-2 flips on once the violation corpus shows a stable
clause-vocabulary catalog (operator-judged sufficiency, not a
percentage).

## Codex handoff scope

### Title

Implement the Pattern Peripheral: transactional `.flexiarg` creation
with parser-validity / clause-vocabulary / sigil / reference checks,
advisory mode default.

### `:in` (READ-ONLY)

- `futon3a/src/futon/flexiarg/projection.clj` — canonical parser
  (must exist; this excursion blocks on P-1 of
  M-pattern-retrieval-calibration shipping).
- `futon3a/resources/notions/pattern-projections.edn` — canonical
  packet store written by P-1.
- `futon3/scripts/sigil_allowlist.clj` (or wherever
  `scripts.sigil-allowlist` lives) — sigil allowlist source.
- `futon3c/src/futon3c/agency/peripheral.clj` (or current
  peripheral-registry namespace) — for peripheral registration
  pattern. *Read first; do not assume the path.*
- `futon3c/src/futon3c/transport/http.clj` — for HTTP route
  registration shape. *Read first; do not assume the path.*
- `futon3/library/structure/block-as-futonic-revolution.flexiarg` —
  reference exemplar of the bespoke-clause case the peripheral must
  surface (in advisory mode) without rejecting.

### `:out` (CREATE)

- `futon3c/src/futon3c/peripheral/pattern_peripheral.clj` —
  peripheral implementation (functions listed below).
- `futon3c/resources/pattern_clause_vocabulary.edn` — initial
  canonical clause-name vocabulary, seeded from the inventory in
  §"Initial vocabulary" below; format is
  `{:canonical #{...} :tolerated #{...} :version "v0"}`.
- `futon3c/test/futon3c/peripheral/pattern_peripheral_test.clj` —
  unit tests (case list below).
- `futon3c/docs/pattern-peripheral.md` — short operator-facing doc:
  endpoint shape, modes, evidence event names, how to override.

### Function signatures (in `pattern_peripheral.clj`)

```clojure
(defn validate-proposal
  "Run G5..G3 gates against a proposal map. Pure function over disk
   reads. Returns {:status :ok | :violation, :violations [...],
   :packet <canonical-packet>}.

   Proposal: {:author string, :target-path string, :content string}.
   Mode is not consulted here; this is the validation core."
  [proposal vocab-edn sigil-allowlist])

(defn resolve-references
  "Run G2: for each :pattern/references entry in the packet, look up
   in pattern-projections.edn. Returns
   {:resolved [...], :unresolved [...]}."
  [packet projections-edn])

(defn land-pattern!
  "Run G1..G0: write file, update projections.edn, emit
   pattern-created evidence. Side-effecting; returns
   {:landed? bool, :evidence-id string, :violations [...]}.

   Mode: :advisory (default) or :enforced.
   On :enforced + :violation status from validate-proposal, refuses
   the transaction (does not write); emits
   pattern-create-refused evidence."
  [proposal opts])

(defn create-pattern-handler
  "HTTP POST /peripheral/pattern/propose handler. Wraps
   validate-proposal + resolve-references + land-pattern!.
   Body: {:author :target-path :content :mode?}."
  [req])
```

### Shapes

```clojure
;; Proposal
{:author     "claude-4"
 :target-path "futon3/library/<ns>/<name>.flexiarg"
 :content    "@flexiarg <ns>/<name>\n@title ...\n..."
 :mode       :advisory   ; or :enforced
 :session-id "..."}

;; Violation entry
{:gate     :G3 | :G2     ; which gate raised it
 :kind     :unknown-clause | :unknown-sigil | :ns-path-mismatch
           | :unresolved-reference
 :detail   "..."         ; short human-readable
 :where    {:clause-name "RATIONALE"}  ; gate-specific
 :severity :warn | :block}             ; advisory uses :warn

;; Evidence body shapes
{"event"         "pattern-created"
 "author"        "..."
 "path"          "..."
 "qname"         "..."
 "parse-status"  "ok"
 "vocab-status"  "ok" | "violations"
 "refs-status"   "all-resolved" | "partial" | "all-unresolved"
 "mode"          "advisory"
 "violations"    [...]                 ; mirrors :violations
 "projection-version" "flexiarg-v0"}

{"event"         "pattern-create-violation"
 ;; same fields; emitted alongside pattern-created in advisory mode
 ;; when there are violations
}

{"event"         "pattern-create-refused"
 ;; same fields; emitted instead of pattern-created in enforced mode
 ;; on violation
}
```

### Test expectations

In `pattern_peripheral_test.clj`:

1. **Happy path, single canonical pattern**: proposal with content
   matching the canonical vocabulary lands; `pattern-created`
   evidence emitted; file exists at target path; projections.edn
   contains the new packet.
2. **Bespoke-clause exemplar (advisory)**: proposal with the
   `block-as-futonic-revolution.flexiarg` content lands but emits a
   `pattern-create-violation` listing
   `:application-to-working-tree` /
   `:application-to-missions` / etc. as `:unknown-clause` warnings.
   File still lands.
3. **Bespoke-clause exemplar (enforced)**: same proposal with
   `:mode :enforced` is refused; `pattern-create-refused` evidence
   emitted; **no file is written, projections.edn is unchanged.**
4. **Parser failure (G4)**: malformed content (e.g. missing
   `@flexiarg` header) is refused regardless of mode; evidence
   carries the parse error.
5. **Namespace / path mismatch (G3)**: content declares
   `@flexiarg foo/bar` but target path is
   `library/baz/bar.flexiarg`; `:ns-path-mismatch` violation;
   refused in `:enforced`, advisory in `:advisory`.
6. **Unknown sigil (G3)**: sigil not in the allowlist;
   `:unknown-sigil` violation.
7. **Unresolved reference (G2)**: `@references [foo/nonexistent]`
   produces an `:unresolved-reference` violation; in advisory the
   pattern still lands with the reference recorded as
   `:reference/state :unresolved` in the packet.
8. **Atomicity on enforced refusal**: simulate a refused
   transaction; assert disk side effects are zero (file does not
   exist, projections.edn unchanged, no embedding-queue entry).

### Criteria checklist

- [ ] All eight tests above pass under `clojure -X:test`.
- [ ] No new `.flexiarg` parser introduced (CCT-1). Imports the
      canonical futon3a parser; does not reimplement
      `split-arg-blocks` / `parse-components` / `extract-meta`.
- [ ] Advisory mode is the default; enforced mode requires explicit
      `:mode :enforced` from the caller.
- [ ] Evidence event names match the shapes above
      (`pattern-created`, `pattern-create-violation`,
      `pattern-create-refused`).
- [ ] HTTP route registered at `POST /peripheral/pattern/propose`.
- [ ] `pattern-peripheral.md` documents endpoint, mode toggle, and
      evidence event names.
- [ ] CCT-1's flexiarg row in
      `futon0/holes/missions/M-the-futon-stack.md` updated to
      reference this excursion as the creation-side gate (after
      P-1 lands the parser, the peripheral closes the
      creation-side loop).

## Initial vocabulary (seed for `pattern_clause_vocabulary.edn`)

Surveyed against the futon3 library; these are clause names that
appear in 5+ patterns or in `block-as-futonic-revolution.flexiarg`:

**Canonical** (lower-cased keys; what the peripheral expects):
`context, if, however, then, because, conclusion, claim, ground,
warrant, backing, qualifier, rebuttal, next-steps, references`.

**Tolerated** (in widespread use; not refused even in enforced
mode):
`bhk-arrow-semantics, gates-as-arrows, composition,
consequences, what-this-adds-to-curry-howard-operational,
application-to-working-tree, application-to-missions,
application-to-agent-evidence, diagnostic, anti-patterns,
composition-with-siblings`.

The tolerated list is permissive on purpose — the v0 catalog
prefers false-negatives (unflagged authoring) over false-positives
(blocking legitimate work). Curating it down lives outside this
excursion; the operator does it by reading the violation corpus.

## Open questions

1. **Where does the canonical vocabulary live in the long run?**
   Phase-1 ships the EDN file. Phase-2 may want to drive it from
   a flexiarg under `futon3/library/pattern-discipline/` so the
   vocabulary is itself a pattern. Decide once a violation corpus
   exists.
2. **Atomic write under git.** Phase-1 writes directly to disk.
   Phase-2 may want to stage as a commit or via a working-tree
   block (per `block-as-futonic-revolution.flexiarg`). Out of
   scope for this excursion.
3. **Re-embedding latency.** The peripheral queues embeddings; the
   actual embed runs against the M-pattern-mining P-8 auto-ingest
   watcher. Coordinate with that probe; do not duplicate the
   re-embed loop here.
4. **Override semantics in enforced mode.** `:mode :advisory` per
   call exists; do we also want a per-author override? Defer
   until enforced mode ships.

## Sequencing

1. **Block on P-1** (M-pattern-retrieval-calibration) — peripheral
   needs the canonical parser as a `:in`.
2. **Codex handoff** — single GitHub issue lifting this excursion's
   §"Codex handoff scope".
3. **Land in advisory mode**, accumulate violation corpus over ~2
   weeks of normal authoring.
4. **Operator review** of violation corpus → vocabulary curation.
5. **Phase-2 flip to enforced** (separate, smaller handoff).

---

*Excursion — Codex handoff companion to M-pattern-retrieval-calibration.*
