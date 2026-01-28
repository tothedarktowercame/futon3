# AIF Trace Semantics

This document specifies the micro-step semantics of a single AIF tick in the
futon2 ant simulation. Each tick follows the **observe → perceive → act** cycle
of active inference.

## Tick Overview

A single `aif-step` call transforms an ant's state through these phases:

```
┌─────────────────────────────────────────────────────────────────────┐
│                         aif-step (one tick)                         │
├─────────────────────────────────────────────────────────────────────┤
│  1. OBSERVE     │  g-observe: sample world state → observation      │
│  2. MODE        │  next-mode: update behavioral mode (FSM)          │
│  3. PERCEIVE    │  perceive: update beliefs μ given observation     │
│  4. AFFECT      │  update-tau: adjust precision based on need/trend │
│  5. POLICY      │  choose-action: compute EFE, select action        │
│  6. EMIT        │  return {:ant :action :G :diagnostics ...}        │
└─────────────────────────────────────────────────────────────────────┘
```

## Phase 1: Observe

**Function:** `ants.aif.observe/g-observe`

Samples the world state from the ant's perspective. Returns an observation map:

```clojure
{:food     0.8    ; food signal at current cell
 :pher     0.3    ; pheromone signal at current cell
 :home-prox 0.2   ; proximity to home (1.0 = at home)
 :enemy-prox 0.1  ; proximity to nearest enemy
 :h        0.45   ; current hunger level (internal)
 :ingest   0.7    ; recent ingestion rate
 :cargo    0.3    ; cargo being carried
 :reserve-home 0.6 ; colony food reserve level
 :dist-home 12.5  ; distance to home in cells
 :white?   0.0    ; on white/unexplored cell?
 :trail-grad [0.1 -0.2] ; pheromone gradient direction
 ...}
```

Key observation channels:
- **Exteroceptive**: food, pher, home-prox, enemy-prox, trail-grad
- **Interoceptive**: h (hunger), ingest, cargo
- **Social**: reserve-home (colony state)

## Phase 2: Mode Transition

**Function:** `ants.aif.affect/next-mode`

Updates the ant's behavioral mode based on observation thresholds. Modes are:

| Mode | Entry Condition | Exit Condition |
|------|-----------------|----------------|
| `:outbound` | default | cargo ≥ 0.60 |
| `:homebound` | cargo ≥ 0.60 | cargo ≤ 0.10 AND at home |
| `:maintain` | near-home AND reserve < 0.20 | reserve ≥ 0.20 OR far-from-home |

Mode affects action costs and EFE term weights.

## Phase 3: Perceive

**Function:** `ants.aif.perceive/perceive`

Updates beliefs (μ) using predictive coding. Computes prediction errors between
expected and observed sensory values, weighted by precision (Π).

```clojure
;; For each sensory channel:
error[k] = (obs[k] - μ.sens[k]) × Π_o[k]

;; Update rule (gradient descent on free energy):
μ.sens[k] ← μ.sens[k] + α × error[k]
```

Returns updated belief state:
```clojure
{:mu {:pos [x y]      ; believed position
      :goal [gx gy]   ; current goal
      :h 0.45         ; believed hunger
      :sens {...}}    ; believed sensory state
 :prec {:Pi-o {...}   ; observation precisions
        :tau 0.8}     ; global precision/confidence
 :trace [...]}        ; gradient descent steps (for debugging)
```

## Phase 4: Affect (Tau Update)

**Function:** `ants.aif.affect/update-tau`

Adjusts precision (τ) based on need violation and hunger trend. This is the
"emotional" modulation of confidence.

```clojure
;; Need error: how far from homeostatic setpoints
need = max(0, hunger - hunger-thresh) + max(0, ingest-thresh - ingest)

;; Hunger trend: rate of change over recent window
dhdt = hunger[now] - hunger[window-steps-ago]

;; Tau update
delta = (need-gain × need) + (dhdt-gain × dhdt) + reserve-term
tau' = clamp(tau + delta, tau-floor, tau-cap)
```

**When tau increases:**
- Hunger is high (need violation)
- Hunger is rising (positive dhdt)
- Colony reserves are low

**When tau decreases:**
- Needs are satisfied
- Colony reserves are healthy

High τ → more reactive/exploitative behavior
Low τ → more exploratory behavior

## Phase 5: Policy Selection

**Function:** `ants.aif.policy/choose-action`

Computes Expected Free Energy (EFE) for each candidate action and selects via
softmax.

```clojure
;; EFE for action a:
G(a) = λ_pragmatic × pragmatic(a)    ; goal-seeking
     + λ_ambiguity × ambiguity(a)    ; uncertainty reduction
     + λ_info      × info-gain(a)    ; epistemic value
     + λ_colony    × colony(a)       ; social/colony benefit
     + λ_survival  × survival(a)     ; self-preservation

;; Action selection (softmax):
P(a) = exp(-τ × G(a)) / Σ exp(-τ × G(a'))
```

Candidate actions: `:forage`, `:return`, `:explore`, `:defend`, `:pheromone`

## Tick Output

`aif-step` returns a rich diagnostic map:

```clojure
{:ant updated-ant           ; the ant with new state
 :action :forage            ; chosen action
 :observation {...}         ; raw observation
 :policy {:action :forage   ; policy details
          :policies {...}   ; EFE per action
          :tau 0.82}
 :perception {:mu {...}     ; updated beliefs
              :prec {...}}
 :G 0.31                    ; EFE of chosen action
 :P 0.42                    ; probability of chosen action
 :diagnostics {:need 0.12
               :dhdt 0.03
               :tau 0.82
               :risk 0.08
               :ambiguity 0.15
               :info 0.04
               :colony 0.02
               :survival 0.02
               :action-cost 0.01}
 :pattern-trace {...}}      ; optional FuLab pattern integration
```

## State Persistence

Between ticks, the ant carries forward:

| Field | Purpose |
|-------|---------|
| `:mu` | Beliefs (position, goal, sensory predictions) |
| `:prec` | Precision weights including τ |
| `:recent` | Sliding window of recent observations |
| `:mode` | Current behavioral mode |
| `:last-action` | Previous action taken |
| `:last-G` | EFE of last action |
| `:need-error` | Current need violation |
| `:dhdt` | Current hunger trend |

## Relation to FuLab Integration

The tick output feeds into FuLab via:

1. **AIF trace evidence**: Aggregated over an episode (see `aif-trace.md`)
2. **Pattern trace**: When cyber-patterns are active, constraint satisfaction
3. **Diagnostics**: Available for real-time HUD display

## See Also

- `docs/aif-trace.md` — Episode-level summary format
- `docs/hunger-tau-coupling.md` — Detailed tau dynamics
- `src/ants/aif/core.clj` — Implementation
