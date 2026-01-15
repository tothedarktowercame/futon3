# AIF Last-Mile Tickets

Goal: make AIF policy sampling operational (not just logging/gating) while preserving auditability.

## Tickets

- [x] AIF-LM-0: Policy contract + action space
  - Summary: define the exact action space to sample (pattern selection, high-level move types), observation vector, belief state, and G/tau derivations in the MUSN context.
  - Acceptance: written spec lists action set, inputs, outputs, and when sampling is authoritative vs advisory; includes seed strategy and override rules.
  - Evidence: docs/aif-policy-contract.md
  - Dependencies: none.

- [x] AIF-LM-1: Sampling implementation (softmax/Gumbel)
  - Summary: implement stochastic sampling over -G/tau with deterministic seeding; expose logits, probabilities, sampled choice.
  - Acceptance: given a fixed seed + inputs, sampled choice is deterministic; logs include G, tau, logits, probabilities, and sampled id.
  - Evidence: /home/joe/code/futon2/src/futon2/aif/adapters/fulab.clj
  - Dependencies: AIF-LM-0.

- [x] AIF-LM-2: Selection path integration
  - Summary: wire sampling into turn selection and multiarg disambiguation; honor low-tau abstain policy.
  - Acceptance: selection path uses sampled choice when allowed; abstain emits explicit policy record and does not auto-select.
  - Evidence: /home/joe/code/futon3/src/futon3/musn/service.clj, /home/joe/code/futon3/src/futon3/musn/router.clj, /home/joe/code/futon3/src/futon3/musn/schema.clj, /home/joe/code/futon3/scripts/pattern-select
  - Dependencies: AIF-LM-0, AIF-LM-1.

- [x] AIF-LM-3: Agent guidance + enforcement
  - Summary: surface sampled choice in HUD and require follow-up justification when the agent deviates.
  - Acceptance: HUD shows sampled choice and probability; deviations log a reason and do not auto-claim use without justification.
  - Evidence:
    - HUD shows probability: `hud.clj:hud->prompt-block` computes and displays `p=X%` for AIF suggestion
    - Deviation detection: `service.clj:detect-deviation` checks if agent's choice differs from AIF suggestion
    - Deviation logging: `turn-select!` emits `:aif/deviation` event with justified? flag
    - Auto-use blocked: `auto-use-policy` checks for unjustified deviation, returns `reason: "unjustified-deviation"`
    - Schema support: `schema.clj:Reason` includes optional `:deviation` field for justification
    - HUD guidance: prompt includes "If selecting a different pattern than AIF suggests, provide a deviation reason"
  - Dependencies: AIF-LM-1, AIF-LM-2.

- [x] AIF-LM-4: Trace/QA harness
  - Summary: add deterministic tests for sampling (tau sensitivity, distribution shape, seed reproducibility).
  - Acceptance: tests prove low tau leads to greedy selection, high tau yields diversity; seeded runs are reproducible.
  - Evidence:
    - Test file: `test/futon3/aif_sampling_test.clj` with 10 tests, 18 assertions
    - Tau sensitivity: `low-tau-is-greedy` (>90% best choice), `high-tau-yields-diversity` (multiple choices)
    - Distribution shape: `softmax-probabilities-sum-to-one`, `softmax-probabilities-order`
    - Seed reproducibility: `seeded-runs-are-reproducible`, `different-turns-produce-different-seeds`, `different-sessions-produce-different-seeds`
    - Abstain policy: `abstain-when-tau-below-threshold`, `no-abstain-when-tau-above-threshold`
    - Explicit override: `explicit-choice-overrides-sampling`
    - Implementation: Proper softmax sampling in `futon2/src/futon2/aif/adapters/fulab.clj`
  - Dependencies: AIF-LM-1, AIF-LM-2.

- [x] AIF-LM-5: Calibration + evaluation
  - Summary: tune G weights, priors, and tau scaling using real runs; document results.
  - Acceptance: report shows before/after behavior on a fixed prompt set; policies align with intended exploration vs exploitation.
  - Evidence:
    - Calibration report: `docs/aif-calibration-report.md`
    - Analysis script: `scripts/analyze_aif_sessions.clj`
    - Baseline analysis: 31 sessions, 38 selections, tau=1.0 pre-sampling
    - Recommended config: tau/min=0.35, tau/max=0.95, tau/min-sample=0.55
    - Test validation: `test/futon3/aif_sampling_test.clj` confirms exploration/exploitation balance
  - Dependencies: AIF-LM-3, AIF-LM-4.
