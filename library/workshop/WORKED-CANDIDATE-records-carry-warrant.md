# WORKED CANDIDATE: records-carry-warrant through the IAD adapter

zai-7 with Joe, 2026-09-14. The adapter contract
(IAD-ADAPTER.md) says `adapt-pattern` returns a candidate with
source-supported / proposed / unresolved content per rule. This file
IS that output, hand-worked, with every dispatch-economy binding
(B1-B7) filled in — so there is something concrete to read, argue
with, and use as the shape for the other six.

The source pattern: futon3/library/workshop/records-carry-warrant.flexiarg
(admitted 4a7f07c; sigil ◈; sha 31fe8840389b).

## The candidate

```clojure
{:pattern/id "workshop/records-carry-warrant"
 :pattern/sha256-prefix "31fe8840389b"
 :institution/candidate-id "inst/records-carry-warrant-v0"
 ;; --- B5: consumer named at creation (no consumer → retire, not explain)
 :consumer {:instance "the evidence append path + every board's
             certificate emission" :position "any emitting agent"}}
 ;; --- the seven rules; each tagged :source-supported / :proposed / :unresolved
 :rules
 {:position
  {:content "emitter (makes a claim), checker (re-derives the warrant),
    operator (the only asserter of new preference)"
   :status :source-supported
   :source-link "+ THEN clause: 'Attach the derivation' names the
                 attacher; @audience names pipeline authors/reviewers"}
  :boundary
  {:content "any artifact offered as progress enters; it leaves by
    carrying derivation + inputs digest + corpus/registry version +
    assumption field when replay is impossible"
   :status :source-supported
   :source-link "+ THEN (the four attachments) and + HOWEVER (the
                 stated-assumption valve) verbatim"}
  :choice
  {:content "the emitter MUST attach; downstream consumers MAY NOT
    cite warrantless claims; MUST-NOT: silent absence of warrant"
   :status :source-supported
   :source-link "+ THEN 'Claims without warrant are typed absences';
                 + conclusion 'must travel with'"}
  :aggregation
  {:content "witness verdicts authorize citation; refusal vetoes it;
             disputes resolve at the operator ruling"
   :status :proposed          ;; the pattern names the witness, not
   :why-proposed "the pattern says who warrants, not who resolves
                  disagreement about a warrant — this is design,
                  listed not smuggled"}
  :scope
  {:content "claims about the record (freshness verdicts, commit
    proposals, associations, forecasts); NOT: private reasoning,
    exploratory notes (those get :not-proven, outside this regime)"
   :status :source-supported
   :source-link "+ IF 'An artifact asserts something about the record'"}
  :information
  {:content "warrant travels WITH the claim to every consumer (the
    whole point); sigil ◈ asserts the regime is in force"
   :status :source-supported
   :source-link "+ conclusion; sigil-registry ◈ semantics"}
  :payoff
  {:content "warrant costs the emitter (replay storage, version
    stamps); warrantlessness costs every future consumer (re-derivation
    or doubt); :not-proven is free and legal"
   :status :source-supported
   :source-link "+ HOWEVER (warrant has cost) + BECAUSE"}}
 ;; --- B2: beyond the asks, listed and priced, never silently credited
 :beyond-asks
 [{:item "aggregation rule content (above)"
   :price "one operator ruling to adopt"
   :why "pattern is silent on dispute resolution"}]
 ;; --- B4: the zero-mass member — the tempting provision NOT grounded
 :zero-mass-member
 {:content "an automatic penalty for warrantless claims (e.g. precision
    drops imposed by the emitter's own tooling)"
  :not-grounded-by "+ HOWEVER explicitly makes :not-proven free; the
    pattern grounds refusal of CITATION, never punishment of the
    emitter"
  :violation-if-present "an adopted sanction the source does not
    carry — the 'Ostrom would say' shape at this pattern's grain"}
 ;; --- B6: refusal pricing
 :refusal-pricing
 {:invalid-source "reject with malformed-flexiarg detail (adapter
    contract)"
  :forecloses "this candidate; the pattern stays uninterpreted until
    a clean source is pinned — foreclosure is total but cheap to
    reverse (re-pin and re-adapt)"}
 ;; --- B7: budget
 :price-estimate {:adapt "one pass (this file, ~1 agent-session
   quarter)" :adopt "one operator ruling" :maintain "per source sha
   change: re-pin + re-diff, minutes"}
 :deferred-disposition :not-deferred  ;; consumer exists and is live
 :gaps ["commissioning of the ◈ citation check (the lint) remains
        B3's review claim, not this candidate's"]}

## How to read it against the bindings

- **B1 containment**: every :source-supported rule cites its clause;
  the aggregation rule CANNOT, so it is :proposed and appears again in
  :beyond-asks — the same content, deliberately visible twice.
- **B2 congruence**: nothing in the institution goes beyond the four
  attachments + the valve + the citation refusal that the pattern's
  THEN/HOWEVER/conclusion actually say; the one addition is priced.
- **B3 non-self-certifying**: this file claims the RULES follow from
  the source; it does NOT claim adoption. "Interpretation-follows" is
  this file's reviewable claim; "adopted" is Joe's alone.
- **B4 falsifiable**: the zero-mass member names the sanction-shaped
  temptation — the most likely over-interpretation of a warrant
  pattern — so its later appearance is a detectable violation.
- **B5 consumer**: the evidence-append path and certificate emission
  are live consumers today (the chip-board certificates instantiate
  this pattern already), so :not-deferred.
- **B6/B7**: refusals name what they foreclose; prices are honest
  quarter-hour-scale estimates, and deferral was available.

## What a full episode would add (not done here)

Entry (a participation record clocking the proposer into pattern-
maintenance), independent review of the "interpretation-follows"
claims (each :source-link checked against the actual clause), Joe's
adoption receipt, and a departure record whose clause-used field cites
which of the seven rules this episode exercised. That sequence is the
IAD-ADAPTER's "worked first candidate" applied here — this file is
step 2 of its sequence (the candidate), before entry/review/adoption.

feedback-reaches-every-participant (⇄) works the same way: its THEN
supplies choice+information almost verbatim (each edge a four-field
channel); its aggregation would be :proposed (who resolves a dead-edge
dispute?); its zero-mass member is probably "broadcast" — the pattern
prices and routes feedback, and broadcast is the ungrounded temptation
its HOWEVER exists to forbid.
