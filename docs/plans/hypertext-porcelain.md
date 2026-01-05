# Hypertext Porcelain: Artifact/Anchor/Link API

**Status**: Planning
**Author**: Claude (with Joe + ChatGPT input)
**Date**: 2026-01-05
**Handoff-ready for**: Codex

## Context

This plan describes "porcelain" (user-facing API) for hypertext linking in futon3. Codex has been building "plumbing" for fulab; this work is orthogonal and can be integrated later.

ChatGPT proposed a shared "graph ops" vocabulary for Pattern Language Graph (PLG) and hypertext linking. After reviewing the futon3 codebase, we found:

- **Exists**: Router dispatch, Malli validation, append-only logs, proof trails, `suggest-links` in semantics.clj
- **Gap**: No first-class artifact/anchor/link entities, no accept/reject workflow for link candidates

**Important**: Link types are canonically defined in futon4 and persisted in futon1. Futon3 should query/cache these rather than redefining them.

## Goal

Enable agents (Codex, Claude, human) to:
1. Register artifacts (code files, devmaps, docs)
2. Define anchors within artifacts (functions, headings, spans)
3. Suggest links between anchors/artifacts
4. Accept/reject link candidates
5. Query the link graph

Demo target: Self-describe the FUTON stack codebase (reflexivity proof-of-concept).

---

## Phase 1: Entity Store

**File**: `src/futon3/hx/store.clj`

Mirrors the pattern from `gap_store.clj`: atoms + append-only EDN log.

```clojure
(ns futon3.hx.store
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; In-memory state
(defonce artifacts (atom {}))  ;; artifact/id -> artifact
(defonce anchors (atom {}))    ;; artifact/id -> [anchor ...]
(defonce links (atom {}))      ;; link/id -> link

;; Persistence
(def log-path "logs/hypertext.edn")
(def lock (Object.))

(defn append-log! [entry]
  (locking lock
    (spit log-path (prn-str entry) :append true)))

(defn load-log! []
  ;; Replay log to rebuild atoms on startup
  ...)
```

### Entity Schemas

**Artifact**:
```clojure
{:artifact/id      "futon3/src/f2/router.clj"  ;; path-based ID
 :artifact/type    :clojure | :devmap | :org | :markdown
 :artifact/title   "Router dispatch"
 :artifact/path    "src/f2/router.clj"
 :artifact/hash    "sha256:..."  ;; for staleness detection
 :artifact/registered  "2026-01-05T..."}
```

**Anchor**:
```clojure
{:anchor/id        "dispatch"
 :anchor/artifact  "futon3/src/f2/router.clj"
 :anchor/kind      :defn | :defmulti | :heading | :span
 :anchor/selector  {:kind :regex :pattern "\\(defn dispatch"}
 :anchor/line      42  ;; optional, for quick navigation
 :anchor/text      "(defn dispatch [router type client envelope] ..."}
```

**Link**:
```clojure
{:link/id          "uuid"
 :link/from        {:artifact/id "..." :anchor/id "..."}
 :link/to          {:artifact/id "..." :anchor/id "..."}
 :link/type        :uses | :implements | :refines | :related
 :link/status      :suggested | :accepted | :rejected
 :link/confidence  0.72
 :link/agent       :codex | :claude | :joe
 :link/rationale   "dispatch calls handle-workday"
 :link/created     "2026-01-05T..."
 :link/decided     "2026-01-05T..."  ;; when accepted/rejected
 :link/decided-by  :joe}
```

### Status: [ ] Not started

---

## Phase 2: CRUD API

**File**: `src/futon3/hx/api.clj`

```clojure
(ns futon3.hx.api
  (:require [futon3.hx.store :as store]))

;; Artifacts
(defn register-artifact! [{:keys [artifact/id artifact/type artifact/path]}]
  ;; Validate, compute hash, store, log
  ...)

(defn get-artifact [id] ...)
(defn list-artifacts [] ...)

;; Anchors
(defn upsert-anchors! [artifact-id anchors]
  ;; Replace all anchors for an artifact
  ...)

(defn get-anchors [artifact-id] ...)

;; Links
(defn suggest-link! [{:keys [from to type confidence rationale agent]}]
  ;; Create link with :status :suggested
  ...)

(defn accept-link! [link-id decided-by]
  ;; Update :status to :accepted
  ...)

(defn reject-link! [link-id decided-by reason]
  ;; Update :status to :rejected
  ...)

(defn list-links
  ([] ...)
  ([{:keys [status artifact-id]}] ...))

(defn candidates []
  ;; Links with :status :suggested
  (list-links {:status :suggested}))
```

### Status: [ ] Not started

---

## Phase 3: Router Integration

**File**: Extend `src/f2/router.clj`

Add new message types to dispatch:

```clojure
:hx/artifact-register  -> hx.api/register-artifact!
:hx/anchors-upsert     -> hx.api/upsert-anchors!
:hx/link-suggest       -> hx.api/suggest-link!
:hx/link-accept        -> hx.api/accept-link!
:hx/link-reject        -> hx.api/reject-link!
:hx/list-artifacts     -> hx.api/list-artifacts
:hx/list-links         -> hx.api/list-links
:hx/candidates         -> hx.api/candidates
```

**File**: Extend `src/f2/ui.clj`

Add HTTP endpoints:

```
GET  /musn/hx/artifacts           ;; list all
GET  /musn/hx/artifacts/:id       ;; artifact + its anchors
GET  /musn/hx/links               ;; all links (query params: status, artifact)
GET  /musn/hx/candidates          ;; suggested links awaiting decision
POST /musn/hx/links/:id/accept    ;; body: {:decided-by :joe}
POST /musn/hx/links/:id/reject    ;; body: {:decided-by :joe :reason "..."}
```

### Status: [ ] Not started

---

## Phase 4: Link Type Vocabulary

**Approach**: Query futon1 for canonical link types on startup; cache locally.

For MVP, hardcode a minimal set that matches futon1/futon4:

```clojure
(def link-types
  #{:uses        ;; A calls/imports B
    :implements  ;; code implements a devmap prototype
    :refines     ;; A is a more specific version of B
    :related     ;; loose association
    :defines     ;; A defines concept B
    :documents   ;; doc describes code
    :applies-pattern})  ;; workday/proof applies a pattern
```

Validator rejects unknown types; agents can propose new types but they go to a review queue.

### Status: [ ] Not started

---

## Phase 5: Self-Description Demo

Register the FUTON stack itself as the demo corpus.

### Step 5.1: Artifact Registration

Register key files:

```
futon3/src/f2/*.clj           ;; transport, router, ui, semantics, codex
futon3/src/futon3/*.clj       ;; checks, workday, pattern_store, gap_store, etc.
futon3/holes/*.devmap         ;; futon0-7 devmaps
futon3/docs/*.md              ;; planning docs
```

Script: `scripts/register_stack.clj`

### Step 5.2: Anchor Extraction

For Clojure files:
- Each `(defn ...)` and `(defmulti ...)` becomes an anchor
- Namespace declaration becomes an anchor

For devmaps:
- Each `! instantiated-by: Prototype N` line becomes an anchor

Script: `scripts/extract_anchors.clj`

### Step 5.3: Link Suggestion

Use LLM (or static analysis) to suggest:
- Function A calls function B → `:uses` link
- Devmap mentions "router.clj" → `:implements` link
- Pattern in checks.clj references pattern ID → `:applies-pattern` link

Script: `scripts/suggest_links.clj`

### Step 5.4: Review Interface

Emacs buffer (or CLI) showing candidates:
- Accept/reject each
- See rationale and confidence
- Filter by artifact or type

### Status: [ ] Not started

---

## File Checklist

| File | Purpose | Status |
|------|---------|--------|
| `src/futon3/hx/store.clj` | Entity storage + persistence | [ ] |
| `src/futon3/hx/api.clj` | CRUD operations | [ ] |
| `src/f2/router.clj` | Add :hx/* dispatch | [ ] |
| `src/f2/ui.clj` | Add /musn/hx/* endpoints | [ ] |
| `resources/schemas/hypertext.edn` | Link type allowlist | [ ] |
| `scripts/register_stack.clj` | Demo: register artifacts | [ ] |
| `scripts/extract_anchors.clj` | Demo: extract anchors | [ ] |
| `scripts/suggest_links.clj` | Demo: LLM link suggestions | [ ] |
| `logs/hypertext.edn` | Append-only event log | (created at runtime) |

---

## Phase 6: Pattern Validation (LLM-in-the-loop)

This is the semantic core — how do we know a suggested link is valid?

### What Needs Validation

1. **Link suggestions**: Does "function A uses function B" actually hold?
2. **Pattern applications**: Does this workday entry actually follow the claimed pattern?
3. **Anchor accuracy**: Does the selector still find the right text after edits?
4. **Type appropriateness**: Is `:uses` the right relation, or should it be `:refines`?

### Validation Approaches

**Structural (rule-based, no LLM):**
- Anchor selector still matches (regex/xpath against current file)
- Link target exists (artifact/anchor IDs resolve)
- Link type in allowlist
- No duplicate links (same from→to→type)

**Semantic (LLM-in-the-loop):**
- Given source context + target context, confirm the relation holds
- Suggest better relation type if proposed one is wrong
- Flag ambiguous cases for human review

### Validation Interface

```clojure
(ns futon3.hx.validate
  (:require [futon3.hx.store :as store]
            [futon3.llm :as llm]))  ;; or whatever the LLM interface is

(defn validate-link
  "Returns {:valid? bool :confidence float :issues [...] :suggested-type kw}"
  [{:keys [from to type rationale]}]
  (let [structural (validate-structural from to type)
        semantic   (when (:valid? structural)
                     (validate-semantic from to type rationale))]
    (merge structural semantic)))

(defn validate-structural [from to type]
  ;; Check: artifacts exist, anchors resolve, type in allowlist
  ...)

(defn validate-semantic [from to type rationale]
  ;; LLM prompt: "Given these two code snippets and the claim that
  ;; A ':uses' B, rate confidence 0-1 and explain."
  (let [from-context (fetch-context from)  ;; artifact text around anchor
        to-context   (fetch-context to)
        prompt       (format-validation-prompt from-context to-context type rationale)]
    (llm/validate prompt)))
```

### LLM Prompt Template

```
You are validating a hypertext link in a codebase.

SOURCE:
File: %s
Anchor: %s
Context:
```
%s
```

TARGET:
File: %s
Anchor: %s
Context:
```
%s
```

CLAIMED RELATION: %s
RATIONALE: %s

Questions:
1. Does this relation accurately describe the connection? (yes/no/partially)
2. Confidence (0.0-1.0)?
3. If not accurate, what relation would be better?
4. Any issues or ambiguities?

Respond as EDN: {:valid? bool :confidence float :suggested-type kw :issues [...]}
```

### Validation Workflow

```
suggest-link!
  → validate-structural (immediate, sync)
  → if structural passes:
      → validate-semantic (LLM call, may be async)
      → store with :status :suggested, :validation {:structural ... :semantic ...}
  → if structural fails:
      → reject immediately with :issues

accept-link!
  → re-validate-structural (in case file changed)
  → if still valid: :status :accepted
  → if stale: flag for re-review

periodic-revalidation
  → for accepted links, check anchors still resolve
  → flag stale links for review
```

### Integration with Existing LLM Infrastructure

Futon3 already has:
- `aob-chatgpt.el` for ChatGPT integration
- Codex/Claude wrappers in `f2/codex.clj`
- Tatami sessions that feed patterns to LLMs

Validation can use the same infrastructure:
- Sync validation via direct API call
- Async validation queued through existing agent wrappers
- Results logged to proof trail (same as `checks.clj`)

### File Checklist Update

| File | Purpose | Status |
|------|---------|--------|
| `src/futon3/hx/validate.clj` | Structural + semantic validation | [ ] |

---

## Integration Notes

### With Codex's fulab plumbing

Codex has built infrastructure for agent orchestration. This hypertext work is orthogonal:
- Plumbing = how agents run, communicate, get approved
- Porcelain = what agents operate on (artifacts, anchors, links)

Integration point: agents use the `:hx/*` message types to register findings and suggest links.

### With futon1

- Link types should ultimately come from futon1's schema
- Accepted links should be published to futon1 via `futon1_bridge.clj`
- This enables cross-futon queries (e.g., "what code implements this devmap prototype?")

### With futon4 (Arxana)

- Arxana already has hyperedge/scholium concepts
- This work is a futon3-native parallel; eventual unification TBD
- For now, treat as separate implementations that can sync via futon1

---

## Next Actions

1. [ ] Create `src/futon3/hx/store.clj` with entity atoms + append-log
2. [ ] Create `src/futon3/hx/api.clj` with CRUD functions
3. [ ] Add `:hx/*` dispatch to router
4. [ ] Add `/musn/hx/*` HTTP endpoints
5. [ ] Write `scripts/register_stack.clj` to bootstrap demo
6. [ ] Test end-to-end: register artifact → add anchors → suggest link → accept

---

## Handoff Notes for Codex

If picking this up:

1. Start with `store.clj` — it's self-contained and mirrors `gap_store.clj`
2. The append-log pattern is already proven; copy from `checks.clj` or `workday.clj`
3. Router integration is straightforward — see existing `:workday` and `:check` handlers
4. For demo, you can use `clojure.java.io` to list files and regex to find `defn` forms
5. Link types are intentionally minimal; don't expand without checking futon1/futon4 schema
