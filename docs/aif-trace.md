# AIF Trace Evidence (Futon2 -> FuLab)

This note explains what the AIF trace evidence is, why it exists, and how it
fits into the Futon3 / FuLab workflow. It is intentionally a thin summary of an
episode so the proof system can reason about AIF runs without storing full
step-by-step traces.

## Why this exists

Futon2 experiments generate AIF episode traces (step results). Futon3 needs a
compact, comparable payload that can be attached to proofs and workday entries
so "why this run looks healthy" is visible in the same place as other evidence.
The AIF trace evidence gives that bridge: it is small, deterministic to compute,
and stable enough to compare across runs.

In other words:
- Futon2: produces rich per-tick traces.
- Futon3: consumes a summary to support proof checks, drift detection, and
  narrative explainers.
- FuLab workflow: the same evidence can sit beside pattern checks, so humans can
  read a proof and understand how the agent behaved.

## Shape

The AIF evidence map is optional. When present, it must match the schema in
`src/futon3/checks.clj`:

```clojure
{:g-mean 0.842
 :tau-range [0.12 0.88]
 :action-counts {:observe 12 :act 4}
 :observation-vector {:test-status :pass
                      :diff-size 42}
 :observation-coverage {:observed 8
                        :total 12
                        :coverage 0.6666666667}
 :precision-registry {:tests 0.9
                      :tool-output 0.6}
 :g-terms {:risk 0.31 :info-gain 0.12} ; optional
 :g-term-channels {:risk {:observation-keys [:test-status]
                          :precision-channels [:tests]}} ; optional
 :g-term-traceability {:terms [:risk :info-gain]
                       :with-provenance [:risk]
                       :missing-provenance [:info-gain]} ; optional
 :constraint-violations ["pattern-constraint-failed@act"]} ; optional
```

Fields:
- `:g-mean` - average of the per-step `:G` values in the episode.
- `:tau-range` - `[min max]` of per-step precision `:tau`.
- `:action-counts` - frequency map of `:action` values.
- `:observation-vector` - optional typed snapshot of the latest observation features.
- `:observation-coverage` - optional summary of how many steps reported observation vectors.
- `:precision-registry` - optional per-channel precision weights for evidence sources.
- `:g-terms` - optional mean value per G term when per-step breakdowns are present.
- `:g-term-channels` - optional term-to-channel summary derived from provenance.
- `:g-term-traceability` - optional coverage of which terms include provenance.
- `:constraint-violations` - optional list of failed constraints (if any).

Unknown fields are rejected by validation.

## How it is computed

The extraction logic lives in `src/futon3/aif_bridge.clj`:
- `:g-mean` is computed from `:G` across all steps.
- `:tau-range` is computed from `[:perception :prec :tau]` (or `[:prec :tau]`).
- `:action-counts` is computed from per-step `:action` values.
- `:observation-vector` is the most recent non-empty normalized observation map.
- `:observation-coverage` counts steps with non-empty observation vectors.
- `:precision-registry` is the most recent non-empty precision registry map.
- `:g-terms` averages the per-step G term breakdown when provided by the engine.
- `:g-term-channels` summarizes observation keys + precision channels seen in term provenance.
- `:g-term-traceability` flags terms missing provenance.
- `:constraint-violations` is derived from `[:pattern-trace :constraint-ok?]`
  when false.

This is intentionally "lossy": it summarizes the episode without needing the
full trace.

## Flow into checks

1) An AIF runner builds a per-episode summary using `futon3.aif-bridge`.
2) The summary is attached as `:aif-trace` on a workday payload or check request.
3) `futon3.checks/check!` includes it in the proof as `:check/aif-trace`.

If it is missing, checks continue as usual.

## Why these metrics

These fields were chosen because they are:
- Stable: aggregate values do not explode when a single tick varies.
- Interpretable: a human can glance at g-mean, tau spread, and actions.
- Minimal: enough to compare runs and spot drift, without storing full traces.

For example, rising g-mean with a narrowing tau range suggests confidence, while
action-counts show whether a policy is shifting in behavior.

## Example (check request -> proof)

```clojure
{:pattern/id "mojo/west"
 :context "AIF episode with 40 ticks"
 :aif-trace {:g-mean 0.842
             :tau-range [0.12 0.88]
 :action-counts {:observe 12 :act 4}
 :observation-vector {:test-status :pass
                      :diff-size 42}
 :observation-coverage {:observed 8
                        :total 12
                        :coverage 0.6666666667}
 :precision-registry {:tests 0.9
                      :tool-output 0.6}}}
```

Proof excerpt:

```clojure
{:proof/id "proof-..."
 :pattern/id "mojo/west"
 :check/aif-trace {:g-mean 0.842
                   :tau-range [0.12 0.88]
 :action-counts {:observe 12 :act 4}
 :observation-vector {:test-status :pass
                      :diff-size 42}
 :observation-coverage {:observed 8
                        :total 12
                        :coverage 0.6666666667}
 :precision-registry {:tests 0.9
                      :tool-output 0.6}}
 :proof/status :ready}
```

## Scope and non-goals

This is not a trace store. If you need the full episode history, keep it in
Futon2 or your experiment log and attach only the summary here.
