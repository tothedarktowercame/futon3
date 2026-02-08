# M-ARXANA: Graph-Structured Persistence for Lab Notebooks

**Type**: Integration
**Status**: Complete (Phases 1-4)
**Depends**: M-WS (WebSocket stream), futon3a integration (bridge mode)
**Related**: `docs/architecture/futon3a-integration.md`, `docs/lab-notebook-contract.md`, `src/futon3/musn/service.clj`

---

## Owner

Claude (implementation already landed; this section exists for scoping hygiene and future follow-ups).

## Scope

### Scope In

- Persist anchors and typed links for lab notebook turns (append-only registries).
- Expose HTTP endpoints for anchor/link create + query.
- Emit anchor/link events on the stream and support Emacs navigation/backlinks.
- Provide a similarity-based link suggestion path (fallback OK when embeddings unavailable).

### Scope Out

- Full embedding-backed semantic linking via futon3a portal (explicitly future enhancement).
- Garbage collection or deletion semantics for sessions/anchors/links.
- Strong link validation workflows (e.g., approval, moderation, provenance review).

## Time Box

Already completed (Phases 1-4). Further work should be split into follow-up missions with explicit time boxes.

## Exit Conditions

- Success criteria remain satisfied in CI and during normal usage.
- Any new follow-ups (embedding portal integration, GC, validation) are split into separate missions.

## Objective

Extend lab notebook persistence with graph-structured storage, enabling:
1. Arxana-style hypertext linking between notebook turns, plans, and artifacts
2. Cross-session references (link this turn to a previous session's insight)
3. Bidirectional navigation in Emacs (follow links, backlinks)
4. Integration with futon3a's embedding pipeline for semantic linking

---

## Context

Currently, MUSN sessions persist as:
- `lab/sessions/:session-id.edn` - Append-only event log
- `lab/musn/:session-id.edn` - Session state snapshots

This is linear and session-scoped. Arxana-style persistence adds:
- Named anchors within turns
- Typed links between anchors (supports, contradicts, extends, etc.)
- Cross-session references
- Semantic clustering via futon3a embeddings

---

## Architecture

### Layer 1: Anchor Registry

Anchors are named points within sessions:

```clojure
{:anchor/id "musn-abc123:turn-7:insight-1"
 :anchor/session "musn-abc123"
 :anchor/turn 7
 :anchor/type :insight           ; :insight, :decision, :question, :artifact
 :anchor/content "The sigil distance metric..."
 :anchor/created #inst "2026-01-27T..."
 :anchor/embedding [...]}        ; Optional: futon3a embedding
```

Storage: `lab/anchors/index.edn` (append-only registry)

### Layer 2: Link Graph

Links connect anchors with typed relationships:

```clojure
{:link/id "link-001"
 :link/from "musn-abc123:turn-7:insight-1"
 :link/to "musn-xyz789:turn-3:decision-2"
 :link/type :supports            ; :supports, :contradicts, :extends, :implements
 :link/note "This confirms the earlier hypothesis"
 :link/created #inst "2026-01-27T..."
 :link/author "claude"}
```

Storage: `lab/links/graph.edn` (append-only edge list)

### Layer 3: Semantic Index (via futon3a)

Embeddings enable "find similar" queries:

```clojure
;; Query: find anchors semantically similar to this text
(futon3a/nearest-anchors "sigil distance metric" {:limit 5})
;; => [{:anchor/id "..." :similarity 0.92} ...]
```

This builds on the existing futon3a bridge (Drawbridge call to portal).

---

## Implementation Phases

### Phase 1: Anchor Recording ✅

**Goal**: Record anchors during MUSN turns

1. ✅ Added `record-anchor!` to `musn/service.clj`
2. ✅ Extended turn events to include optional anchors
3. ✅ Store anchors in append-only index at `lab/anchors/index.edn`

```clojure
(record-anchor! session-id turn :insight "The sigil distance metric..."
                :note "Key finding" :author "agent")
;; => {:ok true :anchor {:anchor/id "session:turn-7:insight-1" ...}}
```

### Phase 2: Link Creation ✅

**Goal**: Create typed links between anchors

1. ✅ Added `create-link!` to `musn/service.clj`
2. ✅ Added HTTP endpoints: `POST /lab/anchor/create`, `POST /lab/link/create`
3. ✅ Emit link events to WebSocket stream (`:arxana/anchor-created`, `:arxana/link-created`)

```clojure
(create-link! "session1:turn-7:insight-1" "session2:turn-3:decision-2"
              :supports :note "This confirms the earlier hypothesis")
;; => {:ok true :link {:link/id "link-abc123" ...}}
```

HTTP endpoints:
- `POST /lab/anchor/create` - Create anchor
- `POST /lab/link/create` - Create link
- `GET /lab/anchors/:session-id` - Get session anchors
- `GET /lab/links/:anchor-id` - Get links for anchor

### Phase 3: Emacs Navigation ✅

**Goal**: Navigate links in Emacs

1. ✅ Extended `fuclient-logs.el` with anchor highlighting (faces: `fuclient-logs-anchor-face`, `fuclient-logs-link-face`)
2. ✅ Clickable anchor links in log buffer
3. ✅ Added `fuclient-logs-show-backlinks` command (keybinding: `b`)
4. ✅ Integrated with `*FuLogs*` buffer - anchor/link events now appear in stream

### Phase 4: Semantic Linking ✅

**Goal**: Auto-suggest links based on semantic similarity

1. ✅ Added `suggest-similar-anchors` - finds anchors with similar content (Jaccard similarity fallback)
2. ✅ Added `suggest-links-for-anchor` - suggests links with type inference based on anchor types
3. ✅ Added `auto-link-anchors!` - automatically creates links above similarity threshold
4. ✅ HTTP endpoints: `POST /lab/suggest-links`, `POST /lab/auto-link`

```clojure
;; Suggest potential links for an anchor
(suggest-links-for-anchor session-id anchor-id :limit 3)
;; => [{:from "..." :to "..." :suggested-type :supports :similarity 0.36} ...]

;; Auto-create links above threshold
(auto-link-anchors! session-id :threshold 0.3 :limit 10)
;; => {:ok true :links-created 6 :links [...]}
```

**Link type inference:**
- insight → decision = `:supports`
- decision → insight = `:implements`
- question → insight = `:extends`
- same types = `:references`

**Future enhancement**: Integrate with futon3a portal for embedding-based similarity instead of Jaccard.

---

## Success Criteria

- [x] Anchors can be recorded during MUSN turns
- [x] Links can be created between anchors (same or cross-session)
- [x] WebSocket stream includes anchor/link events
- [x] Emacs can navigate links (forward and backward)
- [x] Semantic similarity suggests links between anchors

---

## Open Questions

1. **Anchor granularity**: Per-turn? Per-tool-call? User-defined?
2. **Link validation**: Require both anchors to exist, or allow forward references?
3. **Embedding sync**: Compute on create, or batch job?
4. **Garbage collection**: How to handle deleted sessions?

---

## Related Work

- Arxana (hypertext system for mathematical discourse)
- Roam/Obsidian (bidirectional linking in notes)
- Notion databases (typed relations between pages)
- futon5 evidence chains (proof-of-work linking)

---

## Notes

This mission bridges futon3's session-based persistence with futon3a's semantic capabilities, laying groundwork for a knowledge graph of agent work.
