# Excursion: Clause-Vocabulary Survey of `library/`

**Date:** 2026-05-04
**Owner:** Joe (delegated to claude-4)
**Type:** Read-only evidence-gathering excursion.
**Companion to:** `holes/excursions/E-pattern-peripheral.md` (Pattern Peripheral
Codex handoff — its vocabulary EDN seed was approximate; this excursion
replaces approximation with frequency data) and
`holes/missions/M-pattern-retrieval-calibration.md` §D-9.1 (the
`{:name :name-key :slug :text}` clause representation — confirmed
empirically necessary by the case-variance findings below).

## Methodology

Walked `futon3/library/` (and `futon3/holes/` for completeness) recursively;
matched lines of the form `^\s*[!+]\s+([^:]+):\s*` (the same regex the
existing parsers use). For each match, recorded the original clause name,
the lower-cased hyphenated `name-key` per §D-9.1, the file path, and the
prefix (`!` for `! conclusion:`, `+` for sub-clauses).

- **Files scanned:** 919 (`.flexiarg` + `.multiarg`).
- **Total clauses found:** 8,239.
- **Distinct clause-keys:** 251.
- **Prefix split:** `!` 1,044 / `+` 7,195. (`!` is essentially `! conclusion:`
  on every well-formed pattern; the small surplus over file count comes
  from a few patterns with multiple top-level claims.)

Raw tally (with per-key file counts and original-form variants) saved at
`holes/excursions/data/clause-vocabulary-tally-2026-05-04.json`.

## Headline findings

### F-1. The Toulmin-extended canon is empirically universal.

Seven clause-keys cover **82.6 % of all clause occurrences** (6,803 / 8,239)
and appear in 86–97 % of files:

| key            | count | files | % files | top-form    |
|----------------|------:|------:|--------:|-------------|
| `conclusion`   | 985   | 890   | 96.8 %  | `conclusion` |
| `context`      | 1,021 | 882   | 96.0 %  | `context`    |
| `because`      | 1,031 | 862   | 93.8 %  | `BECAUSE`    |
| `next-steps`   | 934   | 811   | 88.2 %  | `NEXT-STEPS` |
| `then`         | 946   | 802   | 87.3 %  | `THEN`       |
| `if`           | 943   | 799   | 86.9 %  | `IF`         |
| `however`      | 943   | 799   | 86.9 %  | `HOWEVER`    |

This is the **canonical core** — required-or-warned in the peripheral's
vocabulary check. It matches `flexiarg_projection.clj`'s
`has-toulmin-slots?` predicate (which checks for IF/HOWEVER/THEN/BECAUSE),
and adds `conclusion` / `context` / `next-steps` as the framing trio that
the original projection's surface check missed.

### F-2. After the core, distribution falls off a cliff.

Bucketed by file-coverage:

| Bucket | Keys | Occurrences | Notes |
|---|---:|---:|---|
| Universal (≥80 % of files) | 7 | 6,803 | The canonical core (F-1). |
| Common (10–80 %) | 2 | 259 | `evidence` (16 % of files), `failure-modes` (10 %). |
| Specialized (1–10 %) | 19 | 659 | Family-specific vocabulary (F-3). |
| Rare (<1 %) | 223 | 518 | Long-tail bespoke clauses (F-4). |

So 8 % of distinct keys account for 83 % of usage; the remaining 92 % of
keys (247 long-tail) account for 14 % of usage. The substrate is heavily
canonical-by-volume, heavily bespoke-by-vocabulary-size.

### F-3. Bespoke vocabulary is **clustered by namespace**, not random.

Per-namespace profile (filtered to namespaces with ≥3 files and
≥5 % non-canonical clause usage):

| Namespace | Files | Non-canon % | Distinctive clauses |
|---|---:|---:|---|
| `ukrns` | 21 | 64 % | `tension`, `compositions`, `check`, `invariant-statement`, `failure-modes` |
| `writing-coherence` | 17 | 67 % | `tension`, `compositions`, `check`, `failure-modes`, `invariant-statement` |
| `peeragogy` | 16 | 67 % | `tension`, `compositions`, `check`, `absence-signals` |
| `collaboration-coherence` | 7 | 67 % | `tension`, `compositions`, `check`, `failure-modes` |
| `repository-transition` | 8 | 70 % | `therefore`, `illustrates`, `instantiated-by`, `lemma`, `indicator` |
| `structure` | 9 | 56 % | `anti-patterns`, `when-to-pluck`, `how-to-answer`, `what-it-guards-against` |
| `futon-theory` | 32 | 55 % | `anti-patterns`, `invariant-statement`, `enforcement`, `failure-modes` |
| `t3` | 12 | 50 % | `mechanism`, `counterfactual`, `applies-when`, `does-not-apply`, `signals` |
| `liberation` | 16 | 42 % | `mundane-content`, `ripens-in`, `transcendent-quality`, `distinguishing-from-mundane` |
| `equity` | 16 | 37 % | `invariant-statement`, `failure-modes`, `ancestors`, `derivation`, `argument` |
| `invariant-coherence` | 8 | 38 % | `anti-patterns`, `invariant-statement`, `related-moves`, `exemplar-instances` |
| `agency` | 8 | 35 % | `evidence (repo history)`, `invariant-statement`, `anti-patterns` |
| `sidecar` | 12 | 32 % | `because->evidence`, `use` |
| `gauntlet` | 8 | 30 % | `evidence`, `failure-modes`, `argument`, `placental-functions` |
| `peripherals` | 4 | 30 % | `evidence`, `failure-modes` |

**At least four sub-canonical clusters are visible:**

1. **The "structural-tension" family** (`ukrns`, `writing-coherence`,
   `peeragogy`, `collaboration-coherence`, partially `t3`):
   `tension / compositions / check` plus invariant clauses. ~80 patterns.
   This is the *structural-tension-as-observation* shape from
   `library/futon-theory/structural-tension-as-observation.flexiarg`
   crystallised as a clause vocabulary.
2. **The "invariant-shape" family** (`futon-theory`, `equity`,
   `invariant-coherence`, `agency`): `invariant-statement / anti-patterns /
   failure-modes / ancestors / derivation`. ~70 patterns.
3. **The "causal-mechanism" family** (`t3`): `mechanism / counterfactual /
   applies-when / does-not-apply / signals`. ~12 patterns.
4. **The "domain-specific" family** (`liberation`, `repository-transition`,
   `structure`): clauses tied to a specific subject matter
   (Buddhist-meditation, repo-state, structural-theory) and not portable
   elsewhere.

The distinction matters for the peripheral: a `structure/` pattern using
`when-to-pluck:` is not violating any sane vocabulary norm — it's using
the namespace's appropriate clause vocabulary. A *single global*
canonical/tolerated split would either (a) blanket-tolerate everything
(weakening the signal) or (b) flag every namespace-specific clause
(noise dominates). The honest peripheral design needs **per-namespace
clause expectations** layered on top of the universal core.

### F-4. The long tail is mostly intentional, not accidental.

223 keys appear in <1 % of files (518 occurrences total). Sampling shows
they're domain-anchored, not typos:
- `liberation/`: `mundane-content`, `ripens-in`, `transcendent-quality`,
  `distinguishing-from-mundane`, `worn-away`, `the-running-circle`.
- `library/eight-fold-path/` (and similar Buddhist-anchored patterns):
  `distinguishing-from-wrong-view`, `distinguishing-from-wrong-mindfulness`,
  `distinguishing-from-wrong-livelihood`, etc. (one per pattern, by design).
- Process patterns: `commitment-procedure`, `reversal-conditions`,
  `test-procedure`.

These should not be refused. They are *expressive vocabulary* the operator
chose to fit the pattern's content. The peripheral's role is to ensure
the canonical core is present and the namespace's family vocabulary isn't
mangled — not to police domain-specific vocabulary.

### F-5. Case/whitespace variance is real and exactly as §D-9.1 anticipated.

Of the 14 widely-used clause-keys, **10 have multiple original-case forms
in the corpus**:

| key | forms | counts |
|---|---|---|
| `because` | `BECAUSE` / `because` | 1031 total |
| `context` | `context` / `CONTEXT` | 1021 total |
| `then` | `THEN` / `then` | 946 total |
| `if` | `IF` / `if` | 943 total |
| `however` | `HOWEVER` / `however` | 943 total |
| `next-steps` | `NEXT-STEPS` / `next-steps` | 934 total |
| `evidence` | `EVIDENCE` / `evidence` | 165 total |
| `status` | `STATUS` / `status` | 109 total |
| `governance` | `GOVERNANCE` / `governance` | 41 total |
| `mechanism` | `MECHANISM` / `mechanism` | 13 total |

This is the empirical confirmation that
**M-pattern-retrieval-calibration §D-9.1's
`{:name :name-key :slug :text}` discipline is necessary, not paranoid**.
A consumer keying off the original form alone would split each of these
ten lookups into two buckets; the structural-tension family lookup for
`tension` would silently miss either `TENSION` or `tension`-keyed
patterns depending on which case-fold pass it ran. P-1 must preserve
both forms; consumers must use `:name-key` for lookups; only `:name`
is for display.

## Curated vocabulary (proposed seed for `pattern_clause_vocabulary.edn`)

Replaces the approximate seed in §"Initial vocabulary" of
`E-pattern-peripheral.md`.

```clojure
{:vocabulary-version "v0-evidence-grounded-2026-05-04"
 :methodology
   "Frequency-tallied across 919 .flexiarg/.multiarg files in
    futon3/library/ + futon3/holes/ on 2026-05-04. Raw data at
    holes/excursions/data/clause-vocabulary-tally-2026-05-04.json."

 ;; Universal canonical: required-or-warn-on-missing across all namespaces.
 ;; Each appears in ≥86 % of files (F-1).
 :universal-canonical
   #{:conclusion :context :because :then :if :however :next-steps}

 ;; Universal tolerated: common enough to be expected; ≥10 % of files.
 :universal-tolerated
   #{:evidence :failure-modes}

 ;; Per-namespace canonical: clauses expected for patterns in this
 ;; namespace family (F-3). Soft signal — informs the violation
 ;; corpus rather than refusing.
 :namespace-canonical
   {;; Structural-tension family (ukrns / writing-coherence / peeragogy /
    ;; collaboration-coherence / partial t3). The tension/compositions/
    ;; check triad descends from
    ;; library/futon-theory/structural-tension-as-observation.flexiarg.
    :ukrns                      #{:tension :compositions :check :invariant-statement :failure-modes}
    :writing-coherence          #{:tension :compositions :check :invariant-statement :failure-modes}
    :peeragogy                  #{:tension :compositions :check :absence-signals}
    :collaboration-coherence    #{:tension :compositions :check :failure-modes}

    ;; Invariant-shape family.
    :futon-theory               #{:anti-patterns :invariant-statement :failure-modes :enforcement}
    :equity                     #{:invariant-statement :failure-modes :ancestors :derivation :argument}
    :invariant-coherence        #{:anti-patterns :invariant-statement :related-moves :exemplar-instances}
    :agency                     #{:invariant-statement :anti-patterns :failure-modes}

    ;; Causal-mechanism family.
    :t3                         #{:mechanism :counterfactual :applies-when :does-not-apply :signals}

    ;; Domain-specific families (not portable; flagged if used outside).
    :liberation                 #{:mundane-content :ripens-in :transcendent-quality :distinguishing-from-mundane}
    :repository-transition      #{:therefore :illustrates :instantiated-by :lemma :indicator}
    :structure                  #{:anti-patterns :when-to-pluck :how-to-answer :what-it-guards-against}}

 ;; Universal tolerated long-tail (≥1 % file coverage, not in a family).
 :universal-tolerated-extras
   #{:status :tension :check :compositions :invariant-statement
     :governance :derivation :ancestors :related-patterns :anti-patterns
     :because->evidence :absence-signals :lean :mechanism
     :counterfactual :applies-when :does-not-apply :signals :use}

 ;; Anything not in any of the above categories is "bespoke".
 ;; Phase-1 (advisory) emits a violation entry; phase-2 (enforced) refuses
 ;; UNLESS the operator has an override. Bespoke is not always wrong —
 ;; the eight-fold-path and meditation patterns have legitimate
 ;; one-per-pattern clauses (F-4) — so the curation review of the
 ;; violation corpus is what flips the canonical-status of any new
 ;; entry.
}
```

## Implications for the Pattern Peripheral excursion

1. **Vocabulary EDN structure must be richer than a flat allowlist.** The
   approximate seed in `E-pattern-peripheral.md` §"Initial vocabulary"
   collapses to two flat sets (canonical + tolerated). The evidence
   demands at least three layers: universal-canonical, universal-tolerated,
   per-namespace-canonical. Update §"Initial vocabulary" of the
   peripheral excursion to point at this survey's curated seed before
   Codex picks up phase-2 (enforced mode).

2. **Phase-1 violation surface should classify, not just count.** The
   advisory-mode evidence record should distinguish:
   - `:unknown-clause-rare` — the clause-key is in the long-tail bespoke
     bucket; informational.
   - `:unknown-clause-cross-namespace` — the clause is in another
     namespace's canonical set but not this one's; might be a misplaced
     pattern.
   - `:unknown-clause-novel` — the clause-key has never been seen in
     the corpus before; strongest signal that vocabulary is drifting.

3. **`block-as-futonic-revolution.flexiarg`'s bespoke clauses
   (`APPLICATION-TO-WORKING-TREE`, `APPLICATION-TO-MISSIONS`,
   `APPLICATION-TO-AGENT-EVIDENCE`, `DIAGNOSTIC`, `ANTI-PATTERNS`,
   `COMPOSITION-WITH-SIBLINGS`)** are mostly already in the universal-
   tolerated-extras set (`anti-patterns`) or are domain-specific
   bespoke (`application-to-*`). The peripheral's advisory mode should
   surface them as `:unknown-clause-novel` (they appear in only 2 files)
   without refusing. This is the operator-judgement case the
   survey is designed to support.

## Implications for P-1 (canonical parser)

None directly — P-1 preserves whatever clause names appear in the source.
The survey informs *consumers* of P-1's output (the peripheral, the
retrieval rationale layer, the typed-slot lift) about what to expect and
how to validate.

The one P-1 nuance worth surfacing: since case variance is real on the
canonical core itself (BECAUSE/because, IF/if, etc.), §D-9.1's
`:name-key` is not optional — without it, hot-word overlap scoring in
M-pattern-retrieval-calibration §M-4's classical features (which uses
`:because-overlap`) fragments by the same factor as the variant count.
That scoring discipline depends entirely on the parser landing the
lower-cased key.

## Open questions

1. **What's the right home for `pattern_clause_vocabulary.edn`?**
   The peripheral excursion places it at
   `futon3c/resources/pattern_clause_vocabulary.edn` (futon3c-local).
   Given CCT-1's instinct to avoid duplication, an alternative is
   `futon3a/resources/notions/pattern_clause_vocabulary.edn` so the
   retrieval surface and the creation surface read the same file.
   Decide before phase-2 enforcement ships.

2. **Should the namespace-canonical map become flexiargs themselves?**
   The futon3 library has clause vocabulary *for patterns about
   patterns* (e.g., the `structural-tension-as-observation` pattern that
   the `tension/compositions/check` family is downstream of). A future
   curatorial move is to express each cluster as a meta-flexiarg under
   `library/pattern-discipline/` so the vocabulary itself is a pattern,
   not a config file. Out of scope here; flag for the peripheral
   excursion's "Open questions §1".

3. **How does this interact with M-pattern-application-diagnostic's
   typed slots?** That mission specs `{:context :tension :move
   :witness-shape :domain}` as the eventual *typed* slot vocabulary,
   not the clause vocabulary. The mapping isn't 1:1 — `:tension`
   appears in both this survey's clause set and M-pattern-application-
   diagnostic's typed slots. The peripheral and the typed-slot lift
   need a shared dictionary that says "the clause `tension:` projects
   to the typed-slot `:tension`." Worth specifying when M-pattern-
   application-diagnostic enters DERIVE.

---

*Excursion — read-only evidence-gathering; output is the curated EDN
seed and the implications for the peripheral excursion's phase-2.*
