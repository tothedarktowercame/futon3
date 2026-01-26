# Eight Gates (八門 Bā Mén)

Energy patterns for navigating tensions, drawn from Tai Chi's eight energies (八勁 bā jìn).

## Core Insight

**Patterns hold tensions, not obstacles.**

The preference model refinement (Mission 2) revealed that IF/HOWEVER fields are not obstacles to eliminate but tensions to navigate. The eight energies provide vocabulary for *how* to engage with these tensions.

Each energy is a mode of relating to incoming force—not fighting it, not collapsing under it, but transforming the relationship.

## Primary Energies (四正 Sì Zhèng)

| Energy | Hanzi | Mode | When to Use |
|--------|-------|------|-------------|
| [Péng](peng-ward-off.flexiarg) | 掤 | Ward off | Expand from rooted base to create space |
| [Lǚ](lu-roll-back.flexiarg) | 捋 | Roll back | Yield and redirect, adhering to force |
| [Jǐ](ji-press.flexiarg) | 擠 | Press | Concentrate force on specific point |
| [Àn](an-push.flexiarg) | 按 | Push | Whole-structure forward motion |

## Secondary Energies (四隅 Sì Yú)

| Energy | Hanzi | Mode | When to Use |
|--------|-------|------|-------------|
| [Cǎi](cai-pluck.flexiarg) | 採 | Pluck | Ground floating energy downward |
| [Liè](lie-split.flexiarg) | 挒 | Split | Separate coupled concerns |
| [Zhǒu](zhou-elbow.flexiarg) | 肘 | Elbow | Close-range action from center |
| [Kào](kao-lean.flexiarg) | 靠 | Lean | Structural presence, trunk energy |

## Mapping to Preference Model

| Preference Field | Energy Relationship |
|-----------------|---------------------|
| **Scope (IF)** | Determines which energies are appropriate—you can't apply the right response without reading the situation |
| **Risks (HOWEVER)** | Forces to redirect (Lǚ), not eliminate. Acknowledge by adhering, not by fighting |
| **Desired (THEN)** | The direction of structural push (Àn) when aligned |

## Mapping to Policies

| Policy | Primary Energy | Character |
|--------|---------------|-----------|
| Exploit | Àn (push) | Aligned forward motion |
| Explore | Lǚ (roll back) | Yielding, learning, redirecting |
| Balanced | Péng (ward off) | Resilient expansion from center |

## The Rooting Principle

All energies require rooting:

> "Péng is born from a good rooting of the feet on the ground."

In pattern terms: **verify scope conditions before engaging with risks or pursuing desired outcomes.** Without grounding, no energy mode works—you either collapse (ungrounded yield) or break (ungrounded resist).

## Implementation

The compass simulation (`src/futon/compass.clj`) uses eight-energy dynamics:

```clojure
(def energy-weights
  {:peng 0.30   ; ward off - default, expand from rooted base
   :lu   0.15   ; roll back - yield and acknowledge risks
   :ji   0.10   ; press - focus on single concept
   :an   0.10   ; push - aggressive forward motion
   :cai  0.10   ; pluck - ground by connecting risk to concept
   :lie  0.08   ; split - trade concept for risk awareness
   :zhou 0.10   ; elbow - small centered adjustment
   :kao  0.07}) ; lean - consolidate multiple risks
```

Each simulation step selects an energy (weighted toward Péng) and applies its characteristic mutation. The energy profile is tracked and reported.

## Future Work

- Add `@energy` field to existing patterns indicating primary engagement mode
- Explore sigil mapping: emoji = energy mode, hanzi = specific variant
- Investigate multi-energy sequences for complex pattern navigation
- Tune energy weights based on policy strategy (exploit → more Àn, explore → more Lǚ)

---

# Security Layer

Security patterns for detecting and handling fabrication templates, using the eight energies.

## Core Insight

**Pattern templates are not self-certifying.**

The same formal structure (summary / context / if / however / then / because / next-steps) can host either:
- A genuine design pattern coupled to feedback and review
- A fabrication template that produces action while blocking learning

This is not anti-pattern (a bad pattern) but **anti-patterning** (an attack on the capacity to pattern).

## Architecture

```
Layer 1: Everyday patterns ──────────── Default trust
                │
                ▼
Layer 2: Tripwires ──────────────────── Passive monitoring
                │ (fires when invariant breaks)
                ▼
Layer 3: Security patterns ──────────── Active escalation
                │
        ┌───────┼───────┐
        ▼       ▼       ▼
    Quarantine  Test   Release/Reject
```

## Tripwires

| Tripwire | Detection Signal |
|----------|------------------|
| harm-is-external | Cost assigned outside pattern boundary |
| dissent-is-threat | Critique reframed as attack |
| self-sealing-logic | Evidence reinterpreted to confirm |
| escalate-on-failure | Doubling down when wrong |
| exit-suppression | Cannot withdraw or disengage |
| review-blocking | Audit channels closed |

When a tripwire fires, escalation begins.

## Primary Security Patterns (四正)

| Pattern | Energy | Action |
|---------|--------|--------|
| [ward-off-boundary](ward-off-boundary.flexiarg) | Péng 掤 | Establish boundary, quarantine |
| [roll-back-hold](roll-back-hold.flexiarg) | Lǚ 捋 | Hold without adopting |
| [press-mechanism](press-mechanism.flexiarg) | Jǐ 擠 | Demand mechanism test |
| [push-warrant](push-warrant.flexiarg) | Àn 按 | Force warrant test |

## Escalation Flow

```
Tripwire fires
      │
      ▼
ward-off-boundary ─────── Quarantine pattern
      │
      ▼
roll-back-hold ─────────── Study without adopting
      │
      ├──► press-mechanism ─── Is BECAUSE causal or decorative?
      │           │
      │           ▼
      └──► push-warrant ────── Does mechanism warrant adoption?
                  │
                  ├──► Pass: Release from quarantine
                  │
                  └──► Fail: Escalate to secondary energies
```

## Two Tests

### Mechanism Test (press-mechanism)
> Is the BECAUSE actually explaining what causes what?

- Decorative BECAUSE: could justify any THEN equally well
- Genuine BECAUSE: constrains which outcomes follow

### Warrant Test (push-warrant)
> Even if the BECAUSE explains attraction, does it warrant adoption?

- Explanation ≠ Warrant
- Must pass ahimsa test: harm visible, bounded, corrigible

## Secondary Security Patterns (四隅)

For exceptional escalation when primary patterns are insufficient:

| Pattern | Energy | Action |
|---------|--------|--------|
| [pluck-extract](pluck-extract.flexiarg) | Cǎi 採 | Extract suspect component |
| [split-isolate](split-isolate.flexiarg) | Liè 挒 | Separate pattern from context |
| [elbow-immediate](elbow-immediate.flexiarg) | Zhǒu 肘 | Close-range intervention |
| [lean-commit](lean-commit.flexiarg) | Kào 靠 | Full escalation, final resort |

## Ahimsa Constraint

> Harm must remain visible, bounded, and corrigible.

This is the legitimacy test for any pattern:
- **Visible**: Those harmed know they are harmed
- **Bounded**: Harm does not scale unboundedly
- **Corrigible**: Those harmed can contest and seek correction

Fabrication templates violate ahimsa by making harm invisible, unbounded, or uncorrigible.

## Connection to Xenotypes

In futon5 terms, these security patterns are the **xenotype layer**:
- Local exotypes: everyday pattern execution (trust by default)
- Xenotypes: security/escalation layer (dormant until tripwire)

The xenotype "bends" all local exotypes when activated, enforcing the security posture across the system.
