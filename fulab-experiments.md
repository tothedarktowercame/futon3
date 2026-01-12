# FuLab Experiments

## E02 prompt set (AIF pattern next-steps)

### E02-A: Belief state deltas (mu-diff + :aif/belief-delta)
- Branch: `e02-belief-delta`
- Prompt (fucodex):
  ```
  Implement the next-step from aif/belief-state-operational-hypotheses: add a mu-diff helper and log :aif/belief-delta events.
  Scope: src/futon3/fulab/pattern_competence.clj (mu-diff helper + event builder), and any minimal call site that emits the delta when mu changes.
  Update evidence in library/aif/belief-state-operational-hypotheses.flexiarg.
  No repo-wide search; ask if you need more scope.
  ```

### E02-B: Precision registry defaults + helper
- Branch: `e02-precision-registry`
- Prompt:
  ```
  Implement the next-step from aif/evidence-precision-registry: add default-precision-registry and a helper to fetch precision by channel.
  Scope: src/futon3/fulab/pattern_competence.clj (default map), src/futon3/aif_bridge.clj (precision-for helper).
  Update evidence in library/aif/evidence-precision-registry.flexiarg.
  No repo-wide search; ask if you need more scope.
  ```

### E02-C: Term provenance validation + dominant channels
- Branch: `e02-term-traceability`
- Prompt:
  ```
  Implement next-steps from aif/term-to-channel-traceability:
  1) extend check-psr in src/futon3/hx/logic.clj to validate :aif/g-terms provenance (term-id, observation-keys, precision-channels, intermediate-values, final-contribution).
  2) add a dominant-channels helper that flags any term where one channel >60%.
  Update evidence in library/aif/term-to-channel-traceability.flexiarg.
  Keep scope to src/futon3/hx/logic.clj plus any small helper you need; no repo-wide search.
  ```
