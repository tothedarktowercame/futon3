# Cycle-Machine Patterns

DRAFT — Claude (Fable 5), 2026-08-23. Derived from the F27 review
(`futon3c/holes/technotes/TN-fable-F27-review.md`): one week of
M-apm-demonstration repairs (~150 APM commits, 2026-08-17..23) fell into
three clusters, each a boundary between the verified cycle model and the
world that had no contract and many producers.

## The problem these patterns address

"Loop through the problems and solve them in order, sharing results with a
student who demonstrates learning" reads as a ten-line phase loop. It is
not. It hides three effects — *solve* (an external process behind a job
lifecycle), *in order* (durable resume across process restarts), *share*
(cross-agent memory) — and each effect is where the leaks were.

The Lean process model (`DarkTower/APMCycleMachine.lean`) verifies
**rulings over observations**. It says nothing about how observations are
*produced*. The shell produced them eight different ways. These patterns
name the producers and make each one singular and contract-tested.

## Structure (per README-flexiarg §5a)

- `single-producer` is the namespace's principle (`@grade principle`); it
  rests on `futon-theory/single-source-of-truth` and on
  `war-room/wr-22-logic-model-before-code`, whose HOWEVER — *a logic model
  cannot settle substrate-dependent guarantees; carve them out* — is this
  namespace's diagnosis. The three ports are the carve-out.
- The three ports `@why single-producer`.
- `step-machine` is the second principle; `runtime-restoration` and
  `disruption-soak` `@why` it, and it carries an editorial `@how` back to
  them (the two methods by which its durability claim is carried out).

## Derivation

```
Level 0 (Meta):     futon-theory ─────────────── exotype
                         | constrains
Level 1 (Domain):   cycle-machine/ ───────────── genotype (these patterns)
                         | instantiates
Level 2 (Code):     futon3c.apm.{port,coordinator} ── phenotype
Level 2 (Spec):     DarkTower/APMCycleMachine.lean  ── adjudicator over port output
```

## Patterns

| Pattern | Port | Leak it closes (evidence) |
|---|---|---|
| [single-producer](single-producer.flexiarg) | (principle) | 8 files × 21 call sites producing one verified structure |
| [job-port](job-port.flexiarg) | Agency job lifecycle | 67010bc7 → f981f441 → d915cce0 → cbb431dd: five theories of announce/invoke in 25 h |
| [authority-port](authority-port.flexiarg) | filesystem roots & revisions | cc753705 … b95dff66 … 11a2f940: nine wrong-root repairs; one silent |
| [toolchain-port](toolchain-port.flexiarg) | Lean elaboration baseline | 2cabe328 → 204c7cdf → 58085cfd → 1e4aba80: baseline widened per problem |
| [step-machine](step-machine.flexiarg) | durable resume | 225f9eda → fe0f89a0 → cc8439cf → 57f6e4f5: `drive!` step vs. phase done |
| [runtime-restoration](runtime-restoration.flexiarg) | shared-JVM code staleness (`@why` step-machine) | 2026-08-22/23 worktree `load-file` clobbered master routes; 16058df7 stale `apm.*` ns after partial restore |
| [disruption-soak](disruption-soak.flexiarg) | the durability claim as a test (`@why` step-machine) | every disruption this week was discovered live, once; none is injected by a test |

## Layering

```
            ┌──────────────────────────────┐
            │  verified adjudicator (Lean) │   rulings over observations
            └──────────────▲───────────────┘
                           │ observations (one producer each)
   ┌───────────┬───────────┴───────────┬───────────────┐
   │ job-port  │ authority-port        │ toolchain-port│   contract-tested ports
   └─────▲─────┴───────────▲───────────┴───────▲───────┘
         │                 │                   │
   ┌─────┴─────────────────┴───────────────────┴───────┐
   │  step-machine (durable coordinator)               │   intents, pre-state digest, postconditions
   └───────────────────────────────────────────────────┘
         │                 │                   │
      Agency            git / fs             lake / lean
```

The step-machine calls ports; ports return observation structures; the
adjudicator rules on them. Nothing else touches Agency, the filesystem
roots, or the toolchain. When an incident occurs, the question is
"which port lied?" — not "which guard is missing?".

## How to validate work against these patterns

Each pattern carries a `+ VALIDATION:` sub-component under its `+ THEN:` —
a checklist of assertions that are mechanically checkable (a grep, a test
name, a file that must exist). Evidence for each pattern is `+ evidence:`
under `+ BECAUSE:`, by futon3c commit sha. An implementation claiming to follow a pattern must go through its
checklist and report each item as met / not met / not applicable, with the
evidence (grep output, test name, path). A review of that work checks the
same list. Unchecked items are findings, not style notes.

## Producers table (to be filled as ports land)

| Verified structure (Lean) | Producer (Clojure) | Contract test |
|---|---|---|
| DispatchObservation | `job-port/await-terminal!` | `job_port_contract_test` |
| PreflightAuthorityObservation | `authority-port/resolve` + existence check | `authority_port_test` |
| sorry baseline `(errors sorryWarnings blockingWarnings)` | `toolchain-port/elaborate` | `toolchain_port_test` |
| MachineState / Receipt / intent | `durable-coordinator` | `disruption_soak_test` |

## Status

Draft patterns only; no phenotype yet. Candidate phenotype homes in
futon3c: `live_preflight_runtime` already holds 5 of the job call sites
and is the natural `job-port`; `durable_coordinator` is the step-machine.
