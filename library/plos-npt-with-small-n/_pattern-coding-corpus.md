# Pattern-theoretic coding of the PLOS small-n / NPT corpus

*Meta-research artifact for the `plos-npt-with-small-n` pattern library. Each
corpus paper coded PRESENT / PARTIAL / ABSENT against the 16 library patterns,
then mined for NOVEL FORCES the library doesn't yet capture. Coding grounded in
full-text JATS XML (Europe PMC / PLOS manuscript XML), not abstracts. Done
2026-05-30 (claude-4, one subagent per paper).*

> Caveat: novel forces are interpretive (the subagents' reading), not verified
> facts — they are CANDIDATES for new patterns, pending ratification. Two
> intermediate web fetches hallucinated content mid-run; all final codings were
> re-grounded on authoritative XML.

## DOI → PMCID map (so we never re-derive these)

| DOI | PMCID | Topic | Framework |
|---|---|---|---|
| 10.1371/journal.pone.0255564 | (read in full earlier) | Glaucoma pathway | NPT |
| 10.1371/journal.pone.0192224 | PMC5830037 | Medication Safety Thermometer | NPT (post-hoc) |
| 10.1371/journal.pone.0195890 | PMC5906013 | ERAS surgery | NPT (Coherence-only) |
| 10.1371/journal.pone.0282612 | PMC10490858 | Stroke rehab trial | NPT (hybrid) |
| 10.1371/journal.pone.0239181 | PMC7494119 | AMBER care bundle | NPT (4-source) |
| 10.1371/journal.pone.0215873 | PMC6476519 | Transgender primary care | NPT + Yin multi-case |
| 10.1371/journal.pone.0312943 | PMC11651607 | Course-leader (rehab HE) | Reflexive TA |
| 10.1371/journal.pone.0334692 | PMC12611103 | Open science in HCI | Inductive TA |
| 10.1371/journal.pone.0297969 | (read in full earlier) | Open research data | a-priori framework |
| 10.1371/journal.pone.0198606 | — | Saturation methods | (ammunition only) |

## Per-paper pattern coverage (16 library patterns)

Counts are PRESENT / PARTIAL / ABSENT across the 7 freshly-coded papers.

| Pattern | Verdict spread (7 papers) |
|---|---|
| abstract-n-method-frame | 5 P · 2 partial |
| small-n-is-a-design-feature | 0 P · 4 partial · 3 absent — **weakly instantiated across corpus** |
| transferability-not-generalisability | 1 P · 4 partial · 2 absent — **most papers say "generalizability", not transfer** |
| framework-spine-with-empirical-ribs | 5 P (absent only in the 2 non-NPT papers) |
| coherence-first-when-using-npt | 4 P (the 4 NPT papers); correctly absent in RTA/inductive/HCI |
| site-case-data-display | 4 P · 3 partial |
| multi-method-triangulation-ladder | 2 P · 2 partial · 3 absent |
| barrier-enabler-strategy-display | 2 P · 3 partial · 2 absent |
| quote-table-not-quote-flood | 2 P · 1 partial · 4 absent — **most papers quote-flood prose** |
| training-role-feature-register | 1 P · 5 partial |
| open-science-phase-register | 0 P · 4 partial · 3 absent |
| venue-register-boundary | 1 P · 6 partial |
| research-questions-stated-early | 3 P · 3 partial · 1 absent |
| coding-reliability-stated | 3 P · 4 partial |
| data-availability-and-prereg | 1 P · 6 partial — **prereg almost always absent; DA usually weak "in the paper" form** |
| coreq-checklist-adherence | 2 P · 5 absent — **checklist adherence is the exception, not the norm** |

**Read:** the 16 patterns are real and recurrent, but several are only *weakly*
present across the corpus (small-n-as-design, quote-tables, prereg, COREQ) —
these are aspirational best-practice the field mostly doesn't follow, which is
itself useful: applying them is differentiation, not table-stakes.

## NOVEL FORCES — clustered by cross-paper recurrence

A force in ≥2 papers is a strong new-pattern candidate; singletons are weaker.

### Cluster A — anti-forcing / inductive-before-deductive warrant  ⟵ recurs (3)
Resolves: "did the named framework just confirm itself / did you force themes
into pre-set categories?" Move: sequence inductive coding first, deductive
framework second, and say so.
- `0192224` forcing-avoidance-as-rigour-claim — "helped to avoid forcing of data into predetermined conceptual categories"
- `0282612` hybrid-coding-reconciliation-protocol — "moving back and forth between deductive and inductive coding"
- `0239181` construct-as-question-battery (inverse: pre-commits the deductive lens as an auditable question table)
**→ Proposed pattern: `inductive-before-deductive-warrant`**

### Cluster B — disconfirmation / counterpoint as finding  ⟵ recurs (3-4)
Resolves: "you cherry-picked confirming quotes" AND "triangulation just asserts
convergence." Move: use a second stream / contradiction to TEST, and make the
divergence itself the finding. Distinct from triangulation-ladder (convergence).
- `0239181` disconfirming-case-discipline + patient-vs-professional-counterpoint
- `0282612` perception-vs-experience-disconfirmation
- `0192224` participant-cognitive-dissonance (intra-subject variant)
**→ Proposed pattern: `disconfirmation-not-just-triangulation`**

### Cluster C — paradigm-matched rigour frame (when COREQ doesn't fit)  ⟵ recurs (2-3)
Resolves: "what's your rigour warrant?" in designs where COREQ is a poor fit.
Move: import a paradigm-appropriate rigour apparatus instead.
- `0215873` yin-quality-criteria-as-rigor-substitute (case-study: Yin's 4 criteria)
- `0312943` reflexivity-as-rigour-substitution + researcher-positionality-roster + theme-as-constructed-not-found (RTA: reflexivity over reliability)
**→ Proposed pattern: `paradigm-matched-rigour-frame`** (with RTA-positionality as a likely sub-pattern)

### Cluster D — scope-shield via companion paper  ⟵ recurs (2) — DIRECTLY answers the salami-slicing question
Resolves: "why is this paper so narrow / where's the rest?" Move: deliberately
narrow scope and hand the remainder to a named companion paper.
- `0195890` ERAS narrowed to the SINGLE NPT construct *Coherence*, shipping the
  barriers/facilitators analysis to a sibling paper: "more fully explored elsewhere [25]"
- (rhymes with existing `open-science-phase-register`, but sharper: it's a
  *defence* of narrow scope, not just phase declaration)
**→ Proposed pattern: `scope-shield-via-companion-paper`** — see scoping note below

### Cluster E — turning thinness into the headline  ⟵ recurs (2-3)
Resolves: "small n / a null / a dropout reads as underpowered failure." Move:
make the absence/convergence the affirmative finding.
- `0215873` convergent-null-as-headline-finding ("no differences across cases" → "primary care is primary care") + replication-logic-defends-small-n
- `0192224` discontinuation-as-load-bearing-finding (couldn't recruit primary-care users → "the tool is not appropriate for these settings")
**→ Proposed pattern: `absence-as-evidence`** (sharper sibling of small-n-is-a-design-feature)

### Cluster F — theory-as-heuristic hedge  ⟵ recurs (2)
Resolves: "you're over-claiming the framework as causal/explanatory." Move:
downgrade it to a "device" / name its limitation then claim your design remedies it.
- `0195890` theory-as-heuristic-hedge ("valuable heuristic device") + interconnectedness-as-pre-emptive-defence
- `0282612` theory-limitation-then-remedy-in-design ("one criticism of NPT… this study addresses this by…")
**→ Proposed pattern: `theory-as-heuristic-not-law`**

### Singletons (interesting, one-off — weaker candidates)
- `in-vivo-title-as-credibility-anchor` (0195890) — participant words as title, re-cited as finding
- `adopter-tier-as-analytic-axis` (0192224) — per-quote EA/LA tag does quasi-comparative work
- `barriers-taxonomy-as-flat-glossary` (0334692) — controlled-vocabulary table decoupled from strategy matrix
- `self-exemplifying-openness-disclosure` (0334692) — open-science paper models/confesses its own practice
- `methodological-vs-data-transparency-split` (0334692) — licenses not sharing raw qualitative data
- `bias-named-then-deferred-to-triangulation` (0334692) — name self-report bias, discharge to future work
- `insider-co-design-credibility` (0312943) — embed the studied role in the design team

## Saturation verdict

**Not saturated.** 7 fresh full-reads surfaced **6 recurring novel forces**
(Clusters A–F, each in ≥2 papers) plus 7 singletons. The library should grow by
~6 patterns. Recurrence — not single sightings — is the warrant; the clusters
are stable across different framework choices (NPT, RTA, inductive, multi-case).

## Note for the T3/T1 scoping decision

The corpus gives a real precedent on both sides:
- **STORM/Choose Life** (BMC, last turn): ONE paper spanning all cascade levels;
  its headline contribution *required* the cross-level span.
- **ERAS `0195890`**: legitimately narrowed to a SINGLE construct and shipped
  the rest to a companion paper (`scope-shield-via-companion-paper`).
So a single-phase T3 paper is publishable **iff** the narrowing is done as an
explicit scope-shield (name the companion/next-phase paper, claim the division
of labour) rather than left as an unexplained gap. That is the difference
between "deliberate scope" and "salami-slicing (QRP)".
