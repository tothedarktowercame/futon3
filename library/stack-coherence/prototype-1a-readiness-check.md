# Prototype 1(a) Stack Readiness Check (Futon0-Futon4, optional Futon5)

This scaffold uses the stack-coherence patterns in this directory to produce a
repeatable readiness check at the Prototype 1 level. It is intentionally
mechanical: fill in each section with evidence links, blockers, and status tags.

## Scope
- Primary: Futon0, Futon1, Futon2, Futon3, Futon4
- Optional push: Futon5 (include only if you want a forward-looking tail)

## Inputs
- Devmaps: `futon3/holes/futon0.devmap` through `futon3/holes/futon5.devmap`
- READMEs: `futon0/README.md` through `futon5/README.md`
- Git history for futon0-futon5
- Test artifacts (if any) for each futon

## Checklist (pattern-driven)

### 1) Stack Blocker Detection (stack-coherence/stack-blocker-detection)
Scan devmaps for declared dependencies and check referenced artifacts.

Template:
```
blocked-by[futonX/prototypeY -> futonZ/prototypeW]
evidence[path-or-commit]
status[ready|not-ready]
```

### 2) Ready/Blocked/Triage Classifier (stack-coherence/ready-blocked-triage)
Classify each devmap clause with evidence and blockers.

Template:
```
status[done|ready|blocked|drift] blocked-by[...] evidence[...]
```

### 3) Stack Evidence Ledger (stack-coherence/evidence-ledger)
For any clause marked done/ready, list concrete evidence.

Template:
```
evidence[path|readme-anchor|commit|test]
unsupported[reason] (only if evidence is missing)
```

### 4) README ⇄ Devmap Sync (stack-coherence/readme-devmap-sync)
Diff README claims vs devmap clauses (names, statuses, evidence).

Template:
```
mismatch[readme-claim -> devmap-clause]
action[update-readme|update-devmap|add-evidence]
```

### 5) Commit Intent Alignment (stack-coherence/commit-intent-alignment)
Correlate commit messages with devmap IDs.

Template:
```
match[commit-sha -> devmap-clause]
drift[commit-claim vs devmap-status]
```

### 6) Staleness Scan (stack-coherence/staleness-scan)
Flag stale narratives or clauses; define a freshness window.

Template:
```
freshness-window[<N> days]
stale[file-or-clause -> newest-artifact]
```

### 7) Futon1 Determinism Gate (stack-coherence/futon1-determinism)
Treat the determinism suite as a hard gate for Prototype 1.

Template:
```
status[pass|fail|not-run]
evidence[ci-link|command-log]
blocked-by[determinism-failure]
```

## Output Template (fill in)

### Futon0
- status[...]
- blocked-by[...]
- evidence[...]
- mismatches[...]
- staleness[...]

### Futon1
- status[...]
- blocked-by[...]
- evidence[...]
- determinism[pass|fail|not-run]
- mismatches[...]
- staleness[...]

### Futon2
- status[...]
- blocked-by[...]
- evidence[...]
- mismatches[...]
- staleness[...]

### Futon3
- status[...]
- blocked-by[...]
- evidence[...]
- mismatches[...]
- staleness[...]

### Futon4
- status[...]
- blocked-by[...]
- evidence[...]
- mismatches[...]
- staleness[...]

### Futon5 (optional)
- status[...]
- blocked-by[...]
- evidence[...]
- mismatches[...]
- staleness[...]

## Exit Criteria (Prototype 1a)
- Every clause in Futon0-Futon4 devmaps has a status with evidence or a named blocker.
- No determinism failures remain in Futon1.
- README claims for Futon0-Futon4 are aligned to devmap statuses or flagged with actions.
- Stale narratives are listed with a refresh pointer.
