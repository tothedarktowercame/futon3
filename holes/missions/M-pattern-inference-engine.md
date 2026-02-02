# Mission: Pattern Inference Engine (PSR/PUR → Transitive Links)

Implement inference rules that derive transitive connections from pattern usage, combining Datalog reasoning with embedding-aware predicates.

## Core Insight

**PSR opens, PUR confirms.**

- PSR (Pattern Selection Record) is speculative - "I'm going to try pattern X"
- PUR (Pattern Use Record) is committed - "I used pattern X, here's the outcome"
- Only mint persistent links when PUR confirms the application

## Inference Chains

### Chain 1: Artifact → Pattern (via confirmed PSR/PUR window)

```
Given:
  PSR(session S, turn T1) --applies-pattern--> pattern:X
  PUR(session S, turn T2) --implements--> pattern:X
  Artifact created in session S, turn T where T1 <= T <= T2

Infer:
  Artifact --guided-by--> pattern:X
```

### Chain 2: Doc → Pattern (via Code)

```
Given:
  Code --guided-by--> pattern:X
  Doc --documents--> Code

Infer:
  Doc --documents-application-of--> pattern:X
```

## Two-Layer Architecture

### Layer 1: Ephemeral State (Agency Runtime Datalog)

In-memory facts for live agent state:

```clojure
;; Asserted on PSR creation
(carrying ?agent-id ?pattern-id ?session ?since-turn)

;; Retracted on PUR creation (or session end)
```

Enables:
- "What patterns is agent X currently working with?"
- "Who's carrying pattern Y right now?"
- Live HUD of patterns in play across agents

### Layer 2: Persistent State (Arxana)

Stored links for confirmed usage:

```clojure
;; Only minted on PUR
(applied ?pur-anchor ?pattern ?outcome)
(guided-by ?artifact-anchor ?pattern)
```

Enables:
- "What patterns influenced this code?" (historical)
- "Show provenance chain for this doc"
- Pattern effectiveness tracking

## The One Cool Trick: Embedding-Aware Predicates

Classic AI requires exact symbolic matching:

```clojure
;; Exact match only
(collaboration-opportunity ?a1 ?a2) :-
  (carrying ?a1 ?pattern)
  (carrying ?a2 ?pattern)  ; must be identical
```

With Futon3a embeddings, we get soft matching:

```clojure
;; Semantic neighborhood
(collaboration-opportunity ?a1 ?a2) :-
  (carrying ?a1 ?p1)
  (carrying ?a2 ?p2)
  (nearby-in-sigil-space ?p1 ?p2)  ; embedding similarity > threshold
```

Example: Agent has 习, Codex has 门. Both are in Prototype0 sigil space, semantically nearby. They can pair even without selecting "the same" pattern.

```
┌─────────────────────────────────────┐
│ Nearby in Sigil Space               │
│                                     │
│   习 ←──0.82──→ 门                  │
│    ↖           ↗                    │
│     0.71    0.68                    │
│        ↘  ↙                         │
│          爻                          │
└─────────────────────────────────────┘
```

## Implementation Phases

### Phase 1: PUR-Triggered Link Materialization

When `link-pur-to-pattern!` fires:
1. Find matching PSR in same session (opens the window)
2. Scan for anchors in session between PSR turn and PUR turn
3. Create `guided-by` links from those anchors to the pattern
4. Mark PSR as "confirmed" (optional: link PUR → PSR)

```clojure
(defn materialize-guided-by-links!
  [session-id pur-turn pattern-id]
  (let [psr-turn (find-psr-turn session-id pattern-id)
        anchors (anchors-in-window session-id psr-turn pur-turn)]
    (doseq [anchor anchors]
      (create-link! (:anchor/id anchor)
                    (str "pattern:" pattern-id)
                    :guided-by
                    :note (format "Inferred: in PSR/PUR window turns %d-%d"
                                  psr-turn pur-turn)))))
```

### Phase 2: Agency Datalog for Ephemeral State

Add Datalog store to Agency runtime:

```clojure
;; agency/datalog.clj
(def schema
  {:carrying {:db/cardinality :db.cardinality/many}})

(defn assert-carrying! [agent-id pattern-id session turn]
  (d/transact! conn [{:agent/id agent-id
                      :carrying {:pattern pattern-id
                                 :session session
                                 :since turn}}]))

(defn retract-carrying! [agent-id pattern-id]
  ...)

(defn agents-carrying-nearby [pattern-id threshold]
  "Find agents carrying patterns within threshold similarity."
  ...)
```

### Phase 3: Embedding-Backed Predicates

Integrate Futon3a similarity into Datalog queries:

```clojure
(defn nearby-in-sigil-space [p1 p2 threshold]
  (let [sim (embedding-similarity p1 p2)]
    (> sim threshold)))

;; Register as external predicate
(d/q '[:find ?a1 ?a2
       :where
       [?a1 :carrying ?p1]
       [?a2 :carrying ?p2]
       [(nearby-in-sigil-space ?p1 ?p2 0.7)]]
     @conn)
```

### Phase 4: Query Engine Unification

Futon3 as unified query layer:

```clojure
;; Query across all sources
(query/run
  '{:find [?pattern ?artifact ?doc]
    :where [(guided-by ?artifact ?pattern)
            (documents ?doc ?artifact)]})
```

## Data Sources

| Source | Facts | Storage |
|--------|-------|---------|
| MUSN | PSR, PUR, anchors | `lab/anchors/`, `lab/links/` |
| Agency | carrying (ephemeral) | In-memory Datalog |
| Futon3a | embedding similarity | Vector index |
| Futon1 | historical entities | XTDB |

## Success Criteria

- [ ] PUR creation triggers `guided-by` link materialization
- [ ] Agency tracks ephemeral `carrying` facts
- [ ] Can query "agents carrying nearby patterns"
- [ ] Pattern backlinks show both direct (PUR) and inferred (guided-by) links
- [ ] Docs traced through code back to patterns

## Related

- `M-graph-unification.md` - Broader graph persistence vision
- `M-arxana-graph-persistence.md` - Anchor/link foundation
- `futon1/apps/graph-memory/` - XTDB Datalog patterns
- `futon3a/` - Embedding index for sigil space

## Design Session Context

Captured from 2026-02-02 session exploring:
1. PSR/PUR → Pattern automatic linking (implemented)
2. Transitive inference for artifacts in PSR/PUR window (designed)
3. Ephemeral vs persistent state split (Agency vs Arxana)
4. Embedding-aware predicates for soft matching (vision)
