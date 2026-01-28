# AIF Golden Traces

This document presents four complete tick traces from the ant simulation, each
illustrating a different behavioral mode. These serve as regression baselines
and explanatory examples.

---

## Trace 1: Forage (Outbound Exploration)

**Context**: Ant is hungry, no food nearby, searching for food sources.

### Initial State
```clojure
{:mode :outbound
 :hunger 0.52
 :cargo 0.0
 :mu {:h 0.50 :sens {:food 0.1 :pher 0.2 :home-prox 0.3}}
 :prec {:tau 0.85}
 :recent [{:obs {:hunger 0.48}} {:obs {:hunger 0.50}} {:obs {:hunger 0.52}}]}
```

### Observation
```clojure
{:food 0.15        ; weak food signal
 :pher 0.25        ; moderate pheromone trail
 :home-prox 0.25   ; moving away from home
 :h 0.52           ; hunger rising
 :ingest 0.20      ; low recent intake
 :cargo 0.0
 :trail-grad [0.3 0.1]  ; gradient points NE
 :white? 1.0}      ; unexplored cell
```

### Mode Transition
- Current: `:outbound`
- cargo (0.0) < cargo-high (0.60) → stays `:outbound`

### Perceive
```clojure
;; Prediction errors (obs - μ.sens):
{:food    {:raw 0.05  :precision 1.0  :weighted 0.05}
 :pher    {:raw 0.05  :precision 0.8  :weighted 0.04}
 :h       {:raw 0.02  :precision 1.1  :weighted 0.022}}

;; Updated beliefs:
{:mu {:h 0.51 :sens {:food 0.12 :pher 0.22 ...}}}
```

### Update-Tau
```clojure
;; Inputs:
need = max(0, 0.52 - 0.45) + max(0, 0.60 - 0.20) = 0.07 + 0.40 = 0.47
dhdt = 0.52 - 0.48 = 0.04 (rising hunger over 3 ticks)
reserve-term = 0.0 (reserve at 0.55, neutral)

;; Calculation:
delta = (0.6 × 0.47) + (0.8 × 0.04) + 0.0 = 0.282 + 0.032 = 0.314
tau' = clamp(0.85 + 0.314, 0.08, 1.5) = 1.164
```

**Commentary**: τ rises significantly due to low ingest (need violation) and
rising hunger trend. The ant becomes more exploitative.

### Policy (EFE Computation)
```clojure
{:forage  {:G 0.28 :pragmatic 0.15 :info 0.08 :survival 0.05}
 :explore {:G 0.35 :pragmatic 0.20 :info 0.12 :ambiguity 0.03}
 :return  {:G 0.72 :pragmatic 0.45 :colony 0.15 :survival 0.12}
 :defend  {:G 0.65 :pragmatic 0.40 :survival 0.25}}

;; Softmax with τ=1.164:
P(:forage)  = 0.48
P(:explore) = 0.31
P(:return)  = 0.12
P(:defend)  = 0.09

;; Selected: :forage
```

**Commentary**: Forage wins because it has lowest G (best expected outcome).
High τ sharpens the distribution toward the greedy choice.

### Tick Output
```clojure
{:action :forage
 :G 0.28
 :P 0.48
 :diagnostics {:need 0.47 :dhdt 0.04 :tau 1.164}}
```

---

## Trace 2: Return (Homebound with Cargo)

**Context**: Ant found food, carrying cargo, heading home.

### Initial State
```clojure
{:mode :homebound
 :hunger 0.35
 :cargo 0.72
 :mu {:h 0.36 :sens {:food 0.8 :home-prox 0.4}}
 :prec {:tau 0.65}
 :recent [{:obs {:hunger 0.38}} {:obs {:hunger 0.36}} {:obs {:hunger 0.35}}]}
```

### Observation
```clojure
{:food 0.05        ; left the food source
 :pher 0.45        ; following trail home
 :home-prox 0.55   ; getting closer
 :h 0.35           ; hunger stable/decreasing
 :ingest 0.65      ; recently ate well
 :cargo 0.72       ; full load
 :dist-home 8.2
 :reserve-home 0.45}
```

### Mode Transition
- Current: `:homebound`
- cargo (0.72) > cargo-low (0.10) AND not at home → stays `:homebound`

### Perceive
```clojure
;; Large prediction error for food (was 0.8, now 0.05):
{:food {:raw -0.75 :precision 1.0 :weighted -0.75}}

;; Updated beliefs shift toward "no food here":
{:mu {:sens {:food 0.58 ...}}}  ; gradual update
```

### Update-Tau
```clojure
need = max(0, 0.35 - 0.45) + max(0, 0.60 - 0.65) = 0 + 0 = 0
dhdt = 0.35 - 0.38 = -0.03 (falling hunger, but we max at 0)
reserve-term = -0.05 (reserve at 0.45, slightly low)

delta = (0.6 × 0) + (0.8 × 0) + (-0.05) = -0.05
tau' = clamp(0.65 - 0.05, 0.08, 1.5) = 0.60
```

**Commentary**: τ drops slightly. Needs are satisfied (good ingest, hunger not
high). The ant is calm, confident in its return behavior.

### Policy
```clojure
{:return  {:G 0.18 :pragmatic 0.10 :colony 0.05 :survival 0.03}
 :forage  {:G 0.52 :pragmatic 0.30 :info 0.15 :survival 0.07}
 :explore {:G 0.58 :pragmatic 0.35 :ambiguity 0.15 :info 0.08}
 :pheromone {:G 0.25 :colony 0.20 :pragmatic 0.05}}

;; Softmax with τ=0.60:
P(:return)   = 0.52
P(:pheromone)= 0.28
P(:forage)   = 0.12
P(:explore)  = 0.08

;; Selected: :return
```

**Commentary**: Return has lowest G due to colony benefit of delivering cargo.
Lower τ means distribution is softer—pheromone has decent probability too.

### Tick Output
```clojure
{:action :return
 :G 0.18
 :P 0.52
 :diagnostics {:need 0 :dhdt 0 :tau 0.60}}
```

---

## Trace 3: Explore (White Space Scouting)

**Context**: Ant has moderate hunger, surrounded by unexplored terrain.

### Initial State
```clojure
{:mode :outbound
 :hunger 0.40
 :cargo 0.05
 :mu {:sens {:food 0.3 :novelty 0.2}}
 :prec {:tau 0.45}
 :recent [{:obs {:hunger 0.39}} {:obs {:hunger 0.40}}]
 :white-streak 3}  ; 3 consecutive white cells
```

### Observation
```clojure
{:food 0.08        ; no food here either
 :pher 0.10        ; weak trail
 :home-prox 0.20
 :h 0.40
 :ingest 0.45      ; moderate
 :novelty 0.85     ; high novelty (unexplored area)
 :white? 1.0
 :white-streak 4}  ; now 4 consecutive
```

### Mode Transition
- Stays `:outbound` (low cargo)

### Perceive
```clojure
;; High novelty signal updates beliefs:
{:novelty {:raw 0.65 :precision 0.6 :weighted 0.39}}
```

### Update-Tau
```clojure
need = max(0, 0.40 - 0.45) + max(0, 0.60 - 0.45) = 0 + 0.15 = 0.15
dhdt = 0.40 - 0.39 = 0.01
reserve-term = 0.0

delta = (0.6 × 0.15) + (0.8 × 0.01) = 0.09 + 0.008 = 0.098
tau' = 0.45 + 0.098 = 0.548
```

**Commentary**: τ rises slightly due to mild need (ingest below threshold).
Still relatively low—exploratory behavior remains viable.

### Policy
```clojure
{:explore {:G 0.22 :info 0.12 :ambiguity 0.08 :pragmatic 0.02}
 :forage  {:G 0.38 :pragmatic 0.25 :info 0.08 :survival 0.05}
 :return  {:G 0.55 :pragmatic 0.35 :colony 0.12 :survival 0.08}
 :defend  {:G 0.62 :survival 0.40 :pragmatic 0.22}}

;; Softmax with τ=0.548:
P(:explore) = 0.45
P(:forage)  = 0.30
P(:return)  = 0.15
P(:defend)  = 0.10

;; Selected: :explore
```

**Commentary**: Explore wins due to high epistemic value (info-gain, ambiguity
reduction). The white-streak and novelty signal make exploration valuable.
Moderate τ allows this epistemic choice despite some need pressure.

### Tick Output
```clojure
{:action :explore
 :G 0.22
 :P 0.45
 :diagnostics {:need 0.15 :dhdt 0.01 :tau 0.548}}
```

---

## Trace 4: Defend (Enemy Encountered)

**Context**: Ant encounters enemy near home, switches to defensive behavior.

### Initial State
```clojure
{:mode :outbound
 :hunger 0.38
 :cargo 0.15
 :mu {:sens {:enemy-prox 0.1}}
 :prec {:tau 0.55}
 :recent [{:obs {:hunger 0.37}} {:obs {:hunger 0.38}}]}
```

### Observation
```clojure
{:food 0.20
 :pher 0.30
 :home-prox 0.70   ; close to home
 :enemy-prox 0.82  ; enemy detected nearby!
 :h 0.38
 :ingest 0.50
 :reserve-home 0.35}  ; colony reserves low
```

### Mode Transition
- Stays `:outbound` (no mode change for combat)
- But enemy-prox will dominate policy

### Perceive
```clojure
;; Large surprise on enemy channel:
{:enemy-prox {:raw 0.72 :precision 0.9 :weighted 0.648}}
```

### Update-Tau
```clojure
need = max(0, 0.38 - 0.45) + max(0, 0.60 - 0.50) = 0 + 0.10 = 0.10
dhdt = 0.01
reserve-term = -0.12 (reserve at 0.35, low)

delta = (0.6 × 0.10) + (0.8 × 0.01) + (-0.12) = 0.06 + 0.008 - 0.12 = -0.052
tau' = 0.55 - 0.052 = 0.498
```

**Commentary**: τ actually drops slightly due to low colony reserves (which
encourages exploration for food). But the enemy-prox will dominate action
selection through the survival term.

### Policy
```clojure
{:defend  {:G 0.15 :survival 0.02 :pragmatic 0.08 :colony 0.05}
 :return  {:G 0.32 :survival 0.18 :colony 0.08 :pragmatic 0.06}
 :forage  {:G 0.48 :pragmatic 0.28 :survival 0.12 :info 0.08}
 :explore {:G 0.55 :info 0.20 :pragmatic 0.25 :ambiguity 0.10}}

;; Softmax with τ=0.498:
P(:defend) = 0.51
P(:return) = 0.27
P(:forage) = 0.14
P(:explore)= 0.08

;; Selected: :defend
```

**Commentary**: Defend wins because the survival term heavily penalizes other
actions when enemy-prox is high. Near home + enemy = defend the colony.

### Tick Output
```clojure
{:action :defend
 :G 0.15
 :P 0.51
 :diagnostics {:need 0.10 :dhdt 0.01 :tau 0.498
               :survival-trigger :enemy-proximate}}
```

---

## Summary Table

| Trace | Mode | Action | τ | Key Driver |
|-------|------|--------|---|------------|
| 1. Forage | outbound | :forage | 1.16 | High need, rising hunger |
| 2. Return | homebound | :return | 0.60 | Cargo delivery, needs satisfied |
| 3. Explore | outbound | :explore | 0.55 | High novelty, moderate τ allows epistemic |
| 4. Defend | outbound | :defend | 0.50 | Enemy proximity, survival term dominates |

## Using These Traces

These traces serve as:

1. **Regression baselines**: If parameters change, compare new traces
2. **Documentation**: Explain AIF behavior to newcomers
3. **Debugging**: When behavior seems wrong, compare to golden cases
4. **Teaching**: Walk through the observe→perceive→act cycle

## See Also

- `docs/trace-semantics.md` — Tick phase details
- `docs/hunger-tau-coupling.md` — τ dynamics
- `docs/aif-trace.md` — Episode-level summary format
