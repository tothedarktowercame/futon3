# Hunger-Tau Coupling

This document explains the relationship between hunger dynamics and precision
(τ) in the AIF ant simulation—specifically when and why `update-tau` fires
relative to belief updates in `perceive`.

## The Core Question

In active inference, precision (τ) modulates the balance between exploitation
and exploration. But when should τ change?

- **Too early**: τ changes before the agent has accurate beliefs
- **Too late**: τ doesn't respond to urgent needs
- **Just right**: τ responds to genuine homeostatic violations

## Execution Order in aif-step

```
1. observe        → raw sensory data
2. mode-update    → behavioral FSM
3. perceive       → belief update (μ), THEN precision update (Π)
4. update-tau     → adjust τ based on need/trend ← AFTER perceive
5. choose-action  → use τ in softmax
```

**Key insight**: `update-tau` runs AFTER `perceive` completes. This means:

- τ adjustment uses the *current* observation directly
- τ does NOT wait for beliefs to converge
- This makes τ a "fast" affective signal, not a "slow" belief update

## Why This Order Matters

### Scenario: Sudden Food Discovery

```
tick N:   hunger=0.6, no food nearby, τ=0.9 (high, exploitative)
tick N+1: ant stumbles onto food, hunger still 0.6 but ingest jumps

Perceive: updates μ.sens[:food] toward observed value
          (gradual, takes several ticks to converge)

Update-tau: sees ingest > ingest-thresh immediately
            → τ drops (need satisfied)
            → next action more exploratory
```

If τ waited for beliefs to converge, the ant would over-exploit the food source
before relaxing.

### Scenario: Rising Hunger (No Food)

```
tick N:   hunger=0.3, τ=0.5 (moderate)
tick N+1: hunger=0.35
tick N+2: hunger=0.42
tick N+3: hunger=0.48 > hunger-thresh

Update-tau tracks dhdt (hunger trend) across the window:
- dhdt = 0.48 - 0.3 = 0.18 (positive trend)
- need = max(0, 0.48 - 0.45) = 0.03
- τ increases: more urgent, exploitative search
```

The dhdt term makes τ anticipatory—it rises before hunger crosses the threshold.

## update-tau Formula

```clojure
(defn update-tau [prec state cfg]
  (let [need (need-error state cfg)           ; immediate need violation
        dh   (max 0 (:dhdt state))            ; positive hunger trend
        reserve (get-in state [:obs :reserve-home])

        ;; Reserve term: colony state affects individual τ
        reserve-term (cond
                       (< reserve 0.2)  -0.18  ; crisis → lower τ (explore for food)
                       (< reserve 0.35) -0.12
                       (< reserve 0.5)  -0.05
                       (> reserve 0.75)  0.08  ; abundant → higher τ (exploit)
                       :else 0.0)

        delta (+ (* need-gain need)
                 (* dhdt-gain dh)
                 reserve-term)]
    (clamp (+ tau delta) tau-floor tau-cap)))
```

### Terms Explained

| Term | Gain | Effect on τ |
|------|------|-------------|
| `need` | 0.6 | ↑ when hunger > thresh OR ingest < thresh |
| `dhdt` | 0.8 | ↑ when hunger rising (anticipatory) |
| `reserve-term` | varies | ↓ when colony reserves low (social signal) |

## Contrast with perceive

`perceive` updates beliefs via gradient descent on prediction error:

```clojure
μ.sens[k] ← μ.sens[k] + α × (obs[k] - μ.sens[k]) × Π[k]
```

This is *slow*—it takes multiple ticks for beliefs to converge to observations.
Learning rate α is typically 0.1–0.3.

`update-tau` directly reads the observation:

```clojure
need = max(0, obs[:hunger] - thresh) + max(0, thresh - obs[:ingest])
```

This is *fast*—τ responds in one tick.

## Design Rationale

This split mirrors biological systems:

- **Beliefs (μ)**: Cortical, slow integration, prediction-based
- **Precision (τ)**: Subcortical/affective, fast, direct sensory access

An ant in danger shouldn't wait for its beliefs to update—it should immediately
become more reactive (high τ).

## When perceive DOES Affect τ

Indirectly, through the sliding window:

1. `perceive` updates μ, which affects future prediction errors
2. Lower prediction errors → lower overall "surprise"
3. Lower surprise → need-error tends to stay low
4. Low need-error → τ drifts toward floor

So converged beliefs lead to stable, low τ over time—but the *immediate*
τ response is observation-driven.

## Configuration Parameters

From `default-aif-config`:

```clojure
{:precision {:tau-floor 0.08      ; minimum τ (maximum exploration)
             :tau-cap 1.5         ; maximum τ (maximum exploitation)
             :need-gain 0.6       ; how much need raises τ
             :dhdt-gain 0.8       ; how much rising hunger raises τ
             :hunger-thresh 0.45  ; hunger level that triggers need
             :ingest-thresh 0.60  ; ingest level below which need triggers
             :tau-reserve-gain 0.6
             :tau-survival-gain 0.5}}
```

## Summary

| Question | Answer |
|----------|--------|
| When does update-tau fire? | After perceive, before choose-action |
| What inputs does it use? | Raw observation + dhdt trend window |
| Why not wait for beliefs? | τ is affective/fast, μ is cognitive/slow |
| What's the effect? | Immediate reactivity to homeostatic violations |

## See Also

- `docs/trace-semantics.md` — Full tick cycle
- `src/ants/aif/affect.clj` — Implementation of update-tau, need-error
- `src/ants/aif/perceive.clj` — Belief update implementation
