# Pattern mining: how the library gets its patterns

How new patterns enter `library/`, written from the mathematics case because
that is the one with three completed rounds and a measured failure. The method
is not specific to mathematics; §7 says which parts move and which do not.

The short version: **patterns are authored, not generated.** There is no miner.
Every one of the 1,114 `.flexiarg` files in this repo was written by a person or
an agent who had just read something and named what it was doing. What *is*
mechanical is everything around the authoring — deciding where to look, deciding
what is missing, and checking afterwards whether the new pattern ever fires.

---

## 1. The artifact

A pattern is a `.flexiarg` file: a named reasoning move with the conditions
under which it applies and the objection it has to survive.

```
@flexiarg math-informal/pass-to-a-subsequence
@title Pass to a Subsequence or Subnet
@sigils [🌀/习]
@keywords subsequence, subnet, extract, compactness, convergent, limit, Bolzano-Weierstrass
@audience mathematicians, analysts, topologists
@tone heuristic
@factor Endurance (khanti)

! conclusion:
  When a sequence does not converge, extract a convergent subsequence and argue from there.

  + context: You need convergence, but the full sequence may not cooperate.
  + IF:      You are in a compact space and the sequence is bounded but not convergent.
  + HOWEVER: The subsequence may converge to something unwanted …
  + THEN:    Invoke compactness (Bolzano–Weierstrass, Alaoglu, Tychonoff) …
  + BECAUSE: Compactness arguments are the engine of analysis …
  + NEXT-STEPS:
    next[Verify the compactness hypothesis that justifies extraction.]
```

The `IF / HOWEVER / THEN / BECAUSE` skeleton is the load-bearing part. `HOWEVER`
is what stops the library becoming a list of slogans: a pattern that cannot
state what might go wrong has not been understood well enough to be written down.

Patterns live in families — `library/math-informal/`, `library/math-strategy/`,
and 83 other directories covering agency, coordination, data-mining and so on.
Families are flat namespaces, not a hierarchy; a "family parent" like
`math-strategy/existence-result` is itself an ordinary pattern that happens to
describe a shape its leaves specialise.

---

## 2. The loop

    author  →  register  →  recognise  →  measure the misses  →  author …

**Author** a pattern from something you have just read.

**Register** it in `resources/sigils/patterns-index.tsv`, one tab-separated row:

    pattern  okipona  truth  rationale  hotwords
    math-informal/argue-by-contradiction  li  今  Argue by Contradiction -> …  contradiction, absurd, suppose, …

The `hotwords` column is the only mechanically load-bearing field. Everything
else documents; hotwords are what makes the pattern *findable*.

**Recognise**: the deterministic retriever (Tier 0 in `futon6/scripts/cas_select.py`)
tokenises a passage and scores each pattern by overlap with `hotwords ∪ title`.
No model. It is a cheap candidate generator, and its recall bounds everything
downstream — a pattern with poor hotwords is invisible no matter how good it is.

**Measure the misses**: the step nobody did for three rounds, and §5 is about
what that cost.

---

## 3. The three rounds

### Round 1 — seeds plus a corpus scan (2026-02-08, 25 patterns)

> Seed patterns authored from first principles; additional patterns discovered
> by LLM scan of 535 PlanetMath `.tex` files across category theory,
> probability, and statistical mechanics.
>
> Extend `patterns-index.tsv` (819→844 rows) with hotword entries so the futon3a
> notions engine can recognise these patterns via classical keyword matching —
> validated by bb tagger achieving 496/535 entry coverage.

Two halves. The **seeds** come from the tradition — Pólya's heuristics, the
standard proof-strategy vocabulary — and cost nothing but writing. The **scan**
reads a corpus and asks what else is in there. The validation step matters as
much as either: 496/535 coverage says the hotwords actually retrieve, which is a
claim about the *index*, not about the patterns.

### Round 2 — hand-tagging a sample (2026-04-27, 10 patterns)

From `holes/excursions/E-math-prototype-pilot.md`:

> Sample of 8 papers … For each: read the BHK-shaped triple; read the typed
> hypergraph; **hand-tag the dominant pattern of mathematical move** — i.e. what
> is the paper *doing*, expressed in pattern language.
>
> Time spent: ≈45 minutes of agent work. Goal: not exhaustive analysis, but
> enough texture to learn whether the slot vocabulary survives contact with real
> content.

This round produced a two-level result: 5 *family parents* naming paper shapes
(existence, characterization, structural-relation, property-of-object) and 5
leaves. The commit is explicit that it added "only … gaps the existing
31-pattern library didn't cover".

Forty-five minutes. This is the cheapest round and it is the template.

### Round 3 — working proofs end to end (2026-06-17, 3 patterns)

> `separate-into-independent-pieces`, `count-over-a-decomposition`,
> `epsilon-of-room` — the analysis-idiom gaps the existing 36 didn't cover,
> found by working 4 APM proofs end-to-end.

Four proofs, three patterns. The highest yield per unit of reading, because
working a proof *end to end* forces you to name every move, including the ones a
skim glides over.

---

## 4. The method, extracted

1. **Choose a source and be explicit about it.** A corpus scan, a sample of
   papers, or a handful of proofs worked end to end. The source determines the
   patterns; see §5.
2. **Read for moves, not topics.** The question is "what is this argument
   *doing*", not "what is it about". `pass-to-a-subsequence` is a move;
   `measure theory` is a topic and would make a useless pattern.
3. **Add only gaps.** Every round so far checked its candidates against the
   existing library and added the residue. This is what keeps the library from
   accumulating synonyms.
4. **Write `HOWEVER` honestly.** If you cannot say what might go wrong, you have
   a slogan.
5. **Register hotwords, then verify retrieval.** A pattern that never retrieves
   is not in the library in any sense that matters.
6. **Measure what still misses**, and feed that into the next round's step 1.

Rounds cost tens of minutes each. The expensive part is not the authoring.

---

## 5. The failure this method has already produced

Two of the three rounds drew on analysis-flavoured sources — PlanetMath's
probability and statistical-mechanics files, and four APM proofs from an
analysis programme. Round 3's commit says so plainly: *"the analysis-idiom
gaps"*.

Nobody ran a round for category theory. In 2026-08 the resulting library was
measured against 818 proof steps of arXiv `math.CT`, and the shape of the
mismatch is exact:

| | |
|---|---|
| patterns in `math-informal` | 39 |
| patterns that ever matched on math.CT | 26 |
| never matched once in 478 verified matches | `estimate-by-bounding`, `induction-and-well-ordering`, `separate-into-independent-pieces` |
| most frequent matches | `transport-across-isomorphism` (64), `find-the-right-abstraction` (50), `verify-universal-property` (43) |

### A bias in this measurement — read before trusting a zero

A pattern scoring zero can mean two different things, and the table above does
not distinguish them.

The recogniser does not run on proof prose. It runs on **IATC-extracted step
texts** — terse restatements of mathematical content like *"morphism μ_k:
Σ^{-(k+1)}S^{m_{k+1}} → M_k"* — produced by an earlier stage that keeps the
mathematics and discards the argumentative connective tissue. Measured on the
same corpus:

| | contains `contradict*` |
|---|---|
| source prose (1,523 candidate passages) | 15 (1.0%) |
| extracted step texts (818) | **0** |

So `argue-by-contradiction` scores zero on math.CT not because category theorists
do not argue by contradiction — they do, at about 1% of passages, and 545
passages carry argumentative connectives — but because **the word never reaches
the recogniser.** Its zero is an artifact of the stage the measurement is taken
at.

This biases the whole table in a specific direction: patterns whose trigger
vocabulary is *argumentative* (`argue-by-contradiction`, `exhaustion-as-theorem`,
`split-into-cases`) are systematically invisible, while patterns whose trigger
vocabulary is *mathematical* (`verify-universal-property`,
`transport-across-isomorphism`) pass through extraction intact. A zero in the
first group is evidence about the pipeline; a zero in the second is evidence
about the corpus.

**Any survey of pattern coverage across areas must therefore run on prose**, not
on extracted steps, or it will rediscover this artifact in every area at once and
mistake it for a finding about mathematics.

Round 3's three analysis-mined patterns score **0, 1 and 1** on math.CT —
`separate-into-independent-pieces` never fires, `count-over-a-decomposition` and
`epsilon-of-room` fire once each. **Two matches from a whole round**, against 478
total. That is the sharpest available reading of what a round is worth outside
the distribution it was mined from, precisely because it is a round whose source
is documented in its own commit message.

(An earlier draft of this section claimed the three never-matched patterns were
*exactly* Round 3's three. They are not — the overlap is one. The tidier claim
was more memorable and false, which is the failure mode this whole document is
about.)

The lesson is not "the library is bad". It is that **a pattern library inherits
the distribution of whatever it was mined from, and nothing in the process makes
that visible.** A library built from analysis will keep passing its own tests on
analysis forever. Step 6 — measure what misses, on the corpus you actually care
about — is the only step that can catch it, and it is the step that was missing.

---

## 6. Running a round for a new domain

1. **Measure first — on prose.** Run the recogniser over the target corpus and
   collect two numbers: which existing patterns never fire, and which passages
   retrieve *no* candidate at all. The second set is the worklist. (For math.CT:
   52 of 818 steps offered no candidate.) Earlier rounds had to find their gaps
   by reading; a measured gap list is strictly better input — provided it is
   taken on source passages, not on extracted steps, for the reason in §5.
2. **Pick sources deliberately, and write down why.** Round 1 used a reference
   corpus (PlanetMath); Rounds 2–3 used primary sources. A reference corpus gives
   breadth and canonical vocabulary; primary sources give the moves people
   actually make. Both, if you can.
3. **Work 4–8 items end to end.** Round 3's yield says the depth matters more
   than the count.
4. **Draft, dedupe against the whole library**, not just the obvious family.
5. **Register hotwords and re-run the recogniser.** New patterns must actually
   retrieve on the passages that motivated them — otherwise you have documented
   a move rather than made it findable.
6. **Re-measure.** The no-candidate rate is the score.

### Deciding what is "core" by measurement rather than intuition

The core/topic boundary is a judgement, and judgements about one's own library
are exactly what §5 shows going wrong. It can be measured instead. Survey every
area — arXiv's `math.*` taxonomy gives 32 of them, already stamped on every paper
— and count, lexically, in how many areas each pattern is detectable. Then set
thresholds:

    core        detectable in ≥ 50% of areas
    mid-tier    ≥ 25%
    area-local  below that, and it belongs to the area it fires in

The proportions fall off roughly Zipf-like, so each level down is half the
previous. This makes "core" a *measured property of the corpus* rather than a
claim about mathematics, and it has a useful consequence: a pattern like
`argue-by-contradiction`, which fires everywhere but weakly, is core precisely
*because* it is thin and ubiquitous — the opposite of the conclusion a
single-area match count would reach.

Two cautions. The survey must run on **prose** (§5), or the argumentative
patterns will read as absent in all 32 areas simultaneously. And detectability is
not usage: a lexical hit says the vocabulary is present, not that the move was
made. It is a good enough proxy for placing a file in a directory, and not good
enough for a claim about how mathematicians reason.

### Hierarchy

Areas will themselves subdivide — CT has groupoids, operads, higher categories,
each with idioms the others do not use. The family naming is a flat prefix
(`math-informal`, `math-informal-CT`, `math-informal-CT-groupoids`), and the
loader matches by prefix, so depth costs nothing mechanically: a new level is a
new directory. What does *not* come free is the threshold ladder above, which is
stated for one level and would need a per-parent denominator to nest — "core
within CT" means ≥50% of CT's subareas, not ≥50% of all mathematics.

---

## 7. What generalises

The mathematics case is the worked example, but nothing above is mathematical.
The transferable parts:

- **Patterns are named moves with a stated objection.** Any domain where people
  argue, decide or build has these — the 85 families here already cover agency,
  coordination, code review and careers.
- **Authoring is the cheap step; targeting is the expensive one.** Every round
  cost under an hour of writing. What determines whether the round is *useful*
  is the choice of source, which is a judgement call, and the gap measurement,
  which is not.
- **The recogniser's hotwords are the interface.** Whatever the domain, the
  library is only as reachable as its index.
- **A library silently inherits its sources' distribution.** This is the general
  finding and the reason to write it down. It applies to any curated vocabulary
  — checklists, taxonomies, controlled terms — and it is invisible from inside:
  the library keeps scoring well on material like the material it came from.
  Only a deliberate measurement against a corpus you did *not* mine can show it.

What does *not* generalise: the `@sigils` / `@factor` annotations are specific to
this stack's iiching/paramita indexing, and the two-level family-parent
arrangement of Round 2 suits domains with recognisable document shapes. Neither
is required to author a useful pattern.

---

## Provenance

- Round 1: `d74acf0` (2026-02-08) · Round 2: `3158cd9` (2026-04-27), method in
  `holes/excursions/E-math-prototype-pilot.md` §Method · Round 3: `0dab340`
  (2026-06-17).
- Recogniser: `futon6/scripts/cas_select.py` (`retrieve`, Tier 0) and
  `futon6/scripts/strategy_recognizer.py`.
- The math.CT measurement: `futon6/holes/TN-cas-tier1-findings.md`.
