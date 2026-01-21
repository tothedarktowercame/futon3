# AIF Policy Contract (MUSN)

This document defines the minimum contract for AIF policy sampling in the MUSN
runner. It is a specification for how we compute a decision, how we sample it,
and how we log it.

## Action space

Primary sampled action:
- `pattern/select` over the candidate pattern IDs for the turn.

Optional secondary action (advisory):
- `move/type` hint: `:read` | `:use` | `:update` (used only to guide the agent,
  not to enforce execution).

Notes:
- We do not attempt to sample the LLM's internal policy. We sample the AIF
  policy for pattern choice and (optionally) a high-level move hint.

## Observation vector (o)

Inputs used to construct the observation:
- Candidate pattern IDs.
- Candidate scores (sigil/glove/combined).
- Per-pattern maturity priors (precision).
- Evidence counts (read/update/implement) where available.
- Intent summary and current turn id (for anchors/forecast).

Observation is logged in a compact form for auditability.

## Belief state (mu)

Tracked belief state:
- `pattern-evidence` counts (read/update/implement).
- `tau-cache` per pattern (latest tau-updated).
- Last known maturity phase per pattern (stub/greenfield/active/settled).

Beliefs are not used to restrict actions directly; they inform G and tau.

## Precision (Pi_o)

Observation precision comes from pattern maturity:
- `:active` -> 0.8
- `:greenfield` -> 0.4
- `:settled` -> 0.9
- `:stub` -> 0.2

When a pattern is not in the catalog, precision defaults to 0.5.

## Expected free energy (G)

We compute G per candidate pattern using the fulab AIF adapter:
- Base score from candidate distance.
- Anchor/forecast terms.
- Evidence bonus/penalty from prior pattern actions.

Lower G is better.

## Policy precision (tau)

Tau derives from uncertainty, in priority order:
- `tau-cache` for known patterns,
- precision priors from maturity,
- score spread across candidates.
Prediction-error proxies (from outcomes) increase uncertainty and lower tau.

Tau is clamped by config (`:tau/min`, `:tau/max`).

## Sampling rule

Given candidates `c_i` and their `G_i`:
- Compute logits `l_i = -G_i / tau`.
- Sample with softmax (or Gumbel-softmax).
- If tau < `tau/min-sample` (default 0.55), switch to explore mode instead of auto-selection.

Sampling must be deterministic under a fixed seed:
- Seed = hash(session-id, turn, candidates).

## Authoritative vs advisory

Authoritative:
- `pattern/select` sampled choice when tau >= threshold.

Advisory:
- `move/type` hint.
- Low-tau abstain -> "ask for clarification".

## Logging requirements

Each decision logs:
- candidates, chosen, logits/probabilities, tau, G range.
- observation summary and precision source.
- belief summary (evidence counts, tau-cache).
- seed and RNG parameters.

## Non-goals (v1)

- Full LLM policy sampling.
- Multi-step planning in the sampler.
- Non-local cross-turn constraints.
