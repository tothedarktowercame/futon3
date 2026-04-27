# Excursion: Old-arxiv block detection — closing the F₂ vintage gap

**Status:** STUB / parked (2026-04-27)
**Owner mission:** `holes/missions/M-pattern-application-diagnostic.md`
**Sister excursion:** `holes/excursions/E-substrate-metrics.md` §1
  (this excursion is the follow-on for F-2's "second-order factor R-3
  doesn't address" — paper-vintage richness).
**Sister mission:** `futon6/holes/missions/M-superpod-mark3.md`
  (out-of-scope for mark3 v0; candidate post-INSTANTIATE follow-on
  if vintage gap matters for downstream consumers).

## Goal

Close some of the **F₂ paper-vintage gap** between regular and mfuton
batches that R-3 (eprint default) does not address: regular batches
on older arxiv-math content cluster at F₂ ≈ 0.13–0.20 even with
eprint mode on, while mfuton batches on modern arxiv content reach
F₂ ≈ 0.70–0.74.

The gap is content-dependent. Older arxiv-math papers (1999–2006
era, the bulk of the regular lineage) typically use **plain TeX** or
**AMS-LaTeX without `\begin{theorem}` environments**. Modern arxiv
papers use structured `\begin{theorem}…\end{theorem}` /
`\begin{proof}…\end{proof}` routinely. The current parser
(`futon6/src/futon6/paper_hypergraph.py`) detects environment-based
blocks; on plain TeX it finds nothing.

But older papers still use **prose conventions** a regex layer can
catch: `Theorem 2.1.`, `Proposition 3.5.`, italicized statement
blocks, `Proof.` paragraphs terminated by `□` / `QED` / `\qed`.

## Approach

**Phase 1 (cheap path — start here):**

1. **Regex pre-pass** that scans the paper's TeX source for prose
   conventions and synthesises *virtual* `\begin{theorem}` /
   `\begin{proof}` markers in a copy of the source.
2. Hand off to the existing parser unchanged. The parser sees
   what looks like a normal LaTeX paper and produces the same
   typed hypergraph it produces for mfuton content.

**Phase 2 (validity gate — pair with phase 1 from the start):**

The regex layer will hallucinate. A sentence like "By Theorem 3, the
result follows" is *not* a theorem statement, but a naive regex on
"Theorem N" will catch it. Without filtering, false positives
pollute the hypergraph and **drop F₂ precision** even as recall
rises. Mitigations to consider:

- Matched block must contain ≥ 1 displayed equation.
- Matched block must be followed within N lines by a `Proof.`
  marker (or vice versa).
- Matched block must appear within K lines of a section heading
  rather than mid-paragraph.
- Required minimum statement length (theorems are usually ≥ 1
  sentence; passing references are usually mid-sentence).

Pick 1-2 gates initially; expand if precision still drops. The
acceptance criterion is "F₂ rises *despite* the precision side
dropping somewhat." If we lift recall by 0.3 and precision drops
by 0.1, F₂ still goes up because β=2 weights recall higher.

**Phase 3 (LLM-aided escalation, deferred):**

For papers that survive phase 1 with no detected blocks, run an LLM
prompt that asks "where in this paper are the theorem statements
and proofs?". Expensive; reserve for high-value papers where the
regex layer fails. Probably not needed for v0.

## Test corpus

- **Positive (claims expected):** 858 papers from `results-005`
  (older arxiv-math, eprint on) that the *current* parser finds
  claims in. The block-detection improvements should at minimum
  not regress F₂ on these papers.
- **Negative (claims not expected):** ≈ 4,142 papers from
  `results-005` whose current parser finds zero claims. The new
  regex layer should ideally lift this set into "with claims" with
  reasonable precision.
- **Hand-checked validation set:** 50 papers from the negative
  set, hand-tagged for theorem/proof presence + structure. Use as
  ground truth for tuning the validity gates.

## Expected payoff

The conservative target: F₂ ≈ 0.40–0.50 on regular older-arxiv
batches (vs current 0.13–0.20), measured against the same
`compute-paper-T.py` script. That's roughly halfway between the
current eprint-on-old floor and the mfuton-class ceiling.

If achieved: every results-001..006 batch becomes substantively
useful for prototype-2 work without requiring re-mining of modern
arxiv content. The ~30K papers in those batches multiply the
prototype-2 substrate by 6×.

If *not* achieved (e.g. the regex pre-pass plateaus at F₂ ≈ 0.25):
useful negative result. We learn that the older arxiv-math content
is structurally different at a level no surface-level regex can
fix, and the right move becomes either (a) accept the smaller
prototype-2 substrate (just mfuton lineage) or (b) escalate to
phase 3 LLM-aided extraction.

## Pitfalls

- **`Theorem` in citation context** — `[3, Theorem 4.2]` is a
  reference to an external theorem, not a statement of one. Regex
  must exclude bracketed and parenthetical citation contexts.
- **Mid-paragraph use** — "By Theorem 3 we have…" is reference, not
  statement. Position-and-line-context gating handles this.
- **TeX macros** — some authors define
  `\newtheorem{thm}{Theorem}` then use `\begin{thm}` instead of
  `\begin{theorem}`. The current parser may already handle this;
  worth checking before adding regex layers.
- **Foreign-language papers** — French / Russian / German older
  papers in arxiv-math will use locale-specific terms. Out of
  scope for v0; flag and skip.

## Files referenced

- `futon6/src/futon6/paper_hypergraph.py` — the current parser
  (env-based block detection).
- `futon6/src/futon6/theorem_extraction.py` — adjacent code that
  may already do prose-convention detection; check before
  duplicating.
- `futon6/scripts/compute-paper-T.py` — the F₂-feeder used to
  measure progress.
- `~/code/storage/mark2/outbox/results-005/output/paper-T.tsv` —
  current baseline.

## Cross-references

- `E-substrate-metrics.md` §1 F-2: the empirical observation that
  motivated this excursion.
- `M-superpod-mark3.md` §Out-of-scope: explicitly defers
  vintage-richness work to a follow-on; this is that follow-on.
- `M-pattern-application-diagnostic.md` §"System invariant — the
  live hypergraph projection": the long-term goal is that *any
  paper added to the corpus becomes immediately retrievable*; the
  block-detection layer is a precondition for older arxiv content
  to participate.
