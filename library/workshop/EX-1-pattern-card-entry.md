# EX-1: pattern-card entry and departure

Proposal by zai-7, reviewed and recorded by codex-25, 2026-09-12. Sources:
Agency job `invoke-1789240513445-20488-f9fb66c7` (original proposal) and coverage
review `invoke-1789242601412-20503-63ed7e2a`. Not launched or commissioned.
This file expands the previously recorded summary in LAYER4-PROCESS.md; it does
not retroactively give earlier tasks entry records or change Joe's pending rulings.

## Question and bounded scope

Can an explicit pattern at entry and an evidenced departure make use of the
pattern inspectable and supply material for its revision? Proposed scope: 3–5
tasks over one week, one or two agents, under a named owner and reviewer.
Joe clarified that clock-in may target an institution and role, with a mission
clock as an optional linked facility. The original proposal assumed mission
clocking; this revision retains the distinction.

## Entry and departure (illustrative record convention)

```clojure
{:experiment :EX-1
 :entry {:id "entry-id" :cue-ref "cue-occurrence-id"
         :actor "agent-id" :institution-ref "institution/version"
         :position :proposer :task-ref "task-or-scope"
         :mission-clock-ref nil
         :pattern {:id "workshop/..." :sha256 "full-source-hash"}
         :psr-ref "selection-receipt"
         :intended-transition {:artifact "path-or-row" :from "state" :to "state"}
         :witness-conditions "named observable and check"
         :refusal-conditions "named refusal or apparatus-fault"
         :pending-decisions [:signature-pending-blocks :signature-builder-terms]}
 :departure {:entry-ref "entry-id" :id "departure-id"
             :observed-use {:status :used :clause "THEN"
                            :application-evidence ["event-or-artifact-ref"]}
             :actual-outcome "observation, not desired result"
             :witness-verdict "receipt or explicit unknown/apparatus fault"
             :uncertainty "limits of the inference"
             :unresolved-work [] :maintenance-proposal-ref nil}}
```

Also admit explicit unused/superseded/abandoned/unknown card outcomes with reasons.
No declaration of use is required when the card was not used. Missing departures
remain visible through entry/departure reconciliation. Full hashes identify source
versions; migration of a file does not rewrite earlier evidence.

## Reuse and missing bindings

Existing: the mission clock/lineage, PSR/PUR/backpack facilities, witness lanes,
PAR, and row accounting. Their existence does not prove that this composition is
wired. The one-active-pattern backpack does not represent concurrent institution
memberships. Proposed: institution participation records, cue correlation,
departure schema, watcher, and a consumer routing observed-use findings into
Layer 4 review. No new shadow clock or separate evidence store is proposed.

Checks must cover immediate and missing dispositions, repeated requests, a
refusal with a reopening condition, unused cards, missing departures, and a
storage failure. Preserve all current gates. Any additional experimental watcher
observes only; observation does not grant permission for otherwise forbidden work.

## Evaluation and return path

Useful evidence includes one complete entry/departure pair, a captured refusal
with its reopening condition, and an observed-use finding supporting a maintenance
proposal. These are proposed evaluation cases, not quotas for manufacturing
refusals or amendments. Report a week with no warranted amendment honestly.
Measure actual capture/review costs; intent classification and substantive review
are not free merely because ID lookup is cheap. Clocking and card selection earn
no completion credit by themselves.

An adopted owner chooses the tasks, scope, due events and reviewer before the
trial. At review, classify what happened and route any warranted proposal through
open-proposals-named-adoption and distinguishing-cases-before-adoption. Sanctions
or cooling-off periods require separate grounds, authority and release conditions.
