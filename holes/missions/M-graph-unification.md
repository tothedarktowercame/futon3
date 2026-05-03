# Mission: Graph Unification (Forum + Lab + Patterns + Code)
Status: archived

Persist forum threads, lab notes, and patterns into futon1's graph-memory/XTDB, creating a unified queryable graph that connects:

- **Forum posts** (proof steps, conversations)
- **Lab notes** (MUSN sessions, anchors, links)
- **Patterns** (from pattern catalog)
- **Code** (files, functions referenced)
- **Docs** (markdown, mission files)

## Context

Currently we have:
- Forum: `lab/forum/threads.edn` + `posts.edn` (local EDN)
- Lab: `lab/activity/`, `lab/anchors/`, `lab/links/` (MUSN sessions)
- Patterns: `resources/sigils/patterns-index.tsv`
- Code/Docs: filesystem, no graph representation

Futon1's graph-memory provides:
- Datascript runtime → Event log → XTDB persistence
- Entity/relation model with versioning
- Temporal queries ("what did we know on date X?")

## Proposed Schema

### Entities

| Type | External ID | Label | Source |
|------|-------------|-------|--------|
| `:forum/thread` | `t-abc123` | Thread title | `futon3/forum` |
| `:forum/post` | `p-xyz789` | Post body (truncated) | `futon3/forum` |
| `:lab/session` | `musn-...` | Session description | `futon3/musn` |
| `:lab/anchor` | `anchor-...` | Anchor text | `futon3/musn` |
| `:pattern` | `ants/hunger-...` | Pattern rationale | `futon3/patterns` |
| `:code/file` | `/path/to/file.clj` | Filename | `futon3/code` |
| `:doc/file` | `/path/to/doc.md` | Doc title | `futon3/docs` |

### Relations

| Type | Src → Dst | Notes |
|------|-----------|-------|
| `:forum/contains-post` | thread → post | Thread membership |
| `:forum/reply-to` | post → post | Reply chain |
| `:forum/applies-pattern` | post → pattern | Pattern as inference rule |
| `:lab/contains-anchor` | session → anchor | Anchor in session |
| `:lab/link` | anchor → anchor | Cross-reference |
| `:pattern/mentioned-in` | pattern → post/anchor | Pattern usage |
| `:code/referenced-by` | file → post/anchor | Code mention |
| `:doc/referenced-by` | file → post/anchor | Doc mention |

## Implementation Steps

### Phase 1: Forum Persistence
1. Create `futon3.forum.persistence` namespace
2. Hook into `create-thread!` / `create-post!`
3. Call futon1 `ensure-entity!` + `upsert-relation!`
4. Map forum IDs ↔ graph UUIDs via `:entity/external-id`

### Phase 2: Lab/MUSN Persistence
1. Hook into MUSN anchor/link creation
2. Persist sessions, anchors, links as entities/relations
3. Connect to patterns when detected

### Phase 3: Pattern Entity Creation
1. Import pattern catalog as graph entities
2. Create `:pattern/related-to` edges between similar patterns
3. Link to forum posts that apply them

### Phase 4: Code/Doc References
1. Extract file paths from posts/anchors
2. Create lightweight file entities
3. Link via `:code/referenced-by` / `:doc/referenced-by`

### Phase 5: Arxana Browser Integration
1. Query graph for connected items
2. Visualize in Emacs (org-roam style?)
3. Navigate: post → pattern → other posts using pattern

## Key APIs (from futon1)

```clj
;; app.store
(ensure-entity! db {:entity/name "post-p-abc"
                    :entity/type :forum/post
                    :entity/external-id "p-abc"
                    :entity/label "Post content..."})

(upsert-relation! db {:relation/type :forum/reply-to
                      :relation/src reply-id
                      :relation/dst parent-id})

;; Automatic: event log → XTDB sync
```

## Success Criteria

- [ ] Forum threads/posts persist to XTDB
- [ ] Lab sessions/anchors/links persist to XTDB
- [ ] Patterns exist as queryable entities
- [ ] Can query: "all posts that used pattern X"
- [ ] Can query: "all patterns used in session Y"
- [ ] Arxana browser shows connected graph

## Foundation

**M-arxana-graph-persistence.md** (Complete - Phases 1-4) established:
- Anchor recording within MUSN turns
- Typed links between anchors (supports, contradicts, extends)
- Cross-session references
- Semantic similarity suggestions
- Emacs navigation

This mission **extends** that work by:
- Moving from local EDN → futon1 graph-memory/XTDB
- Adding forum threads/posts as graph entities
- Treating patterns as first-class linkable entities
- Including code/doc references in the graph

## Related

- `futon1/apps/graph-memory/` - Graph implementation (Datascript + XTDB)
- `futon3/src/futon3/futon1-bridge.clj` - Existing HTTP bridge
- `futon3/src/futon3/musn/service.clj` - Anchor/link implementation
