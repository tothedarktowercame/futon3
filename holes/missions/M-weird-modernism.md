# Mission: Weird Modernism — A Futon-Theoretic Reading of PKD

**Status:** IDENTIFY + MAP (2026-05-22, perpetual-projection mode — no closure phase scheduled)
**Owner:** Joe
**Cross-refs:**
- `library/futon-theory/wyrd.flexiarg` — pattern landed 2026-05-22;
  this mission is the operational follow-on, exercising wyrd against
  a primary text (PKD 1978) to expose where the pattern needs
  sharpening, extension, or new sibling patterns.
- `library/futon-theory/futonic-logic.flexiarg` — three-Norn extension
  (既/化/應/債, the 鹽/債 ledger) landed 2026-05-22 in §1, §2.7, §2.8,
  §3.1, §4.12-4.14, §6. The reading uses this vocabulary throughout.
- `library/futon-theory/retroactive-canonicalization.flexiarg` —
  PKD's anamnesis-via-gold-fish-sign moment is a personal-scale
  analogue of library canonicalization; one candidate upgrade is to
  generalise canonicalization across granularities (library, agent,
  biography, civilisation).
- `library/futon-theory/structural-tension-as-observation.flexiarg`
  — the AIF framing that the PKD essay's "secret love of universes
  that fall apart" elaborates at cosmological scale.
- `library/futon-theory/proof-path.flexiarg` — the substrate where
  PKD's "small refusals of the authentic human" become infrastructure
  rather than martyrdom.
- Primary text: Philip K. Dick, "How to Build a Universe That Doesn't
  Fall Apart Two Days Later" (1978 speech, Metz). Full text held in
  conversation; not currently checked into the repo.
- The Operator's Foreword to the FUTON stack — under draft; the
  cold-open invokes the PKD title as a companion to "futonic
  discipline." The foreword is **one projection surface** of this
  mission, not its closure.
- **Parr T, Friston KJ (2018) "Generalised free energy and active
  inference: can the future cause the past?"** bioRxiv 304782.
  PDF at `~/Downloads/304782.full.pdf`. This paper provides the
  formal mechanism (generalised free energy as path integral; future
  treated as hidden state with priors; Hamilton's Principle of
  Stationary Action) that operationally grounds the three-Norn
  decomposition and the Skuld≈SORRY identification. See the
  MAP-phase section below.
- `~/npt/working-paper/UKRN_WP_draft_v10a.md` §3 (and ancestor
  drafts) — independent triangulation in a non-Futon domain. Joe's
  UKRN-Services simulation explicitly frames *"Engagement at the
  institution side is a path integral"* and uses a four-term
  EFE-scorecard (risk / ambiguity / pattern firing / priority
  alignment) for per-tick decisions. Same mathematical structure
  as Parr-Friston, applied to a different operational domain. The
  triangulation — same structure surfacing in Friston's neuroscience,
  Joe's UKRN simulation, AND the futon stack's apparatus — is itself
  a wyrd-survival test passing for the path-integral framing.
- `futon7/holes/M-war-machine-aif-last-mile.md` — the WM AIF
  apparatus already operationalises a sorries.edn schema v2 with
  `:kind` field, plus per-channel R3 likelihoods and an EFE-scorecard
  scoring decisions against priors. The Parr-Friston grounding
  explains *why* the WM design works: sorries-as-priors ARE the
  priors-over-outcomes in the generalised-free-energy formulation.
  This mission does not retake WM scope; it provides the theoretical
  reading WM's R-criteria already practise.
- `futon3/holes/missions/M-live-geometric-stack.md` — substrate-2's
  `(T, ∇, Δ, drift)` quantities are likely implementing some form
  of free-energy minimisation. MAP-phase work below flags the
  hypothesis that these map onto generalised-free-energy components
  (T ~ residual free energy; ∇ ~ variational derivative; etc.).
  Investigation deferred to a follow-on excursion.
- `library/aif/` — sixteen patterns currently encode the
  expected-free-energy approach (e.g. `expected-free-energy-scorecard`,
  `free-energy-as-tick-scalar`, `belief-aware-risk-term`). MAP-phase
  flags an audit: which patterns need updating to the *generalised*
  formulation (preferences explicit in the generative model rather
  than absorbed into policy prior). Audit deferred to a follow-on
  excursion.
- **Buddhist Magga (the Noble Eightfold Path)** — structural prior
  art. The mission lifecycle in `futon4/holes/mission-lifecycle.md`
  (HEAD → IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE
  → DOCUMENT) maps one-to-one onto sammā-diṭṭhi / saṅkappa / vācā /
  kammanta / ājīva / vāyāma / sati / samādhi. The correspondence
  was recognised (not engineered) on 2026-05-22 evening after Joe's
  recent addition of HEAD pushed the lifecycle to eight phases. See
  MAP-phase §"The Eightfold Path correspondence" below. Joe's
  framing: *"Buddha Dhamma feels quite modern, specifically because
  it is not dogmatic"* — the Dhamma's ehipassiko ("come and see")
  empirical stance is structurally aligned with combining-methods-
  as-diagnostic and the refusal of "mere X" closure that futon
  theory practises.

## Goal

Develop upgrades to futon theory by reading PKD's 1978 essay through
the existing futon-theoretic vocabulary, and let the reading generate
candidate refinements, new patterns, and sharpened anti-patterns.

In keeping with the wyrd-modernism framing the mission is named for,
there is **no single definitive closure**. The mission is a structure
that projects onto multiple surfaces simultaneously. The Operator's
Foreword is one such projection surface; the mission file itself is
another; any future patterns derived from this reading are further
surfaces. The projection surfaces can develop in parallel and cite
each other, in the wyrd-rotation discipline (PIE *wert-/vertere*) of
*turning between containers without collapsing into any one*.

## Methodology note: the mission as a wyrd-engineered artifact

The mission inherits its own pattern. Per the three-Norn frame
(`futonic-logic.flexiarg` §2.7):

- **既 (Urðr):** what is already in the library — the wyrd pattern, the
  three-Norn extension, the cross-linguistic salarium/鹽-債/Skuld
  triangulation. The HEAD content below freezes the present reading
  into citeable 既 for future projection surfaces to draw on.
- **化 (Verðandi):** the foreword presently being drafted, the live
  reading conversation between Joe and the agent, the candidate
  upgrades being evaluated.
- **應/債 (Skuld):** the open threads listed at the bottom of this
  file — pattern promotions deferred, refinements not yet made,
  the speculative "have I rediscovered something PKD already
  conceived?" question outstanding.

The mission stays IDENTIFY as long as projection surfaces are still
being developed; it does not need to move to a closure phase, because
some of its load-bearing work is in the perpetual rotation between
surfaces. This is consistent with wyrd-modernism: the mission models
its own subject matter.

## Projection surface 1: Operator's Foreword intro (working draft, 2026-05-22)

> I call it "futonic discipline". Philip K. Dick called it "How to
> build a universe that doesn't fall apart two days later". Indeed,
> I suspect that I only 'rediscovered' futon theory while
> subconsciously reflecting on his work. Anyway, what's certainly
> true is that the advent of agentic coding has allowed me to turn
> a bunch of ideas that I had in my 20s and 30s into working
> prototypes. Back then, I'm not sure anyone knew what was coming,
> but the speculative artefacts I created — a prototype version of
> a Ted Nelson style hypertext system, sketches of a comprehensive
> mathematics dictionary, and so forth — have now started to become
> a lot more real.
>
> As a little contribution to the field of speculative writing, I
> conceptualised "futon theory" as a way of working with the *atoms
> of the future* that exist in the present. The idea was that there
> are symbolic invariants that support diachronic creativity (the
> work I was doing in my 20s was still relevant 20 years later;
> "Rome remains Rome", and so forth). A somewhat less fanciful way
> to talk about this idea is to notice that it's possible to identify
> the concepts and activities that are generative in the present,
> and to devote time and energy to developing them. Obviously there
> are risks involved, as anyone who has been affected by railway or
> tulip fever would attest. Nevertheless, it's reasonably possible
> work with such material rigorously, with a kind of animal husbandry
> applied to memetics. Futonic discipline brings some of the ideas
> of evolutionary computing into *software engineering practice*.
> This way of thinking and working has led me to develop a technical
> apparatus that I can use to understand the evolving state of a
> complex software system. Indeed, one tantalisingly-close possible
> realisation of these ideas, the software system can be largely
> self-guiding, noticing its own points of tension, and repairing
> them.

Annotations on this draft (non-blocking, for Joe's later
consideration):

- "Rome remains Rome" is doing wyrd-survival work directly — it is
  the same trope PKD lands on with *"if we are really living in the
  Roman Empire, somewhere in Syria, why do we see the United States?"*
  An optional move: make the resonance explicit by citing PKD's line
  later in the foreword.
- "atoms of the future that exist in the present" is the wyrd-survival
  criterion in another voice — configurations that the future will
  need but which are presently composable. Worth treating as a
  load-bearing phrase rather than throwaway colour.
- "this such material" probably wants to be "such material" (minor
  typo; corrected in the quote above).
- The arc — discipline-name → PKD-name → personal speculation →
  biographical context → "atoms of the future" definition → risk
  acknowledgment ("railway or tulip fever") → operational frame
  → self-guiding aspiration — is well-shaped. No structural rewrite
  warranted; copy-edits only.

## Projection surface 2 (HEAD): Futon-theoretic reading of PKD 1978

### Thesis

PKD's 1978 essay is a futon-theoretic primer written 47 years before
the vocabulary existed. His two named topics ("what is reality?" /
"what is the authentic human?") are the futon stack's two foundational
questions, and his methodological discipline — building universes
that fall apart, fake fakes, refusing to collapse coincidences into
a single regime — is wyrd-engineering practised without the name.
The essay's central event (the Flow My Tears chain) is a worked
example of wyrd-salience that he could not theorise in 1978 but could
refuse to dismiss. Futon theory is one operationalisation of that
refusal.

### 1. The two questions ARE the two questions

PKD: *"The two basic topics which fascinate me are 'What is reality?'
and 'What constitutes the authentic human being?'"*

His provisional answer to the first: *"Reality is that which, when
you stop believing in it, doesn't go away."*

This is the wyrd-survival criterion verbatim. Belief is a containment
regime; reality is salience generated across all of them. PKD's
definition IS futon-theory/wyrd's diagnostic: salience persisting
across rival containment regimes. He never named it that way because
he was practising it.

His answer to the second: *"The authentic human being is one of us
who instinctively knows what he should not do, and, in addition, he
will balk at doing it. He will refuse to do it, even if this brings
down dread consequences."*

This is 戒 (precept/restraint, futonic-logic §8) at the level of
personhood. The authentic human is the one in whom 應
(modal-obligation, the "should not") can fire even against
state/market/media pressure to collapse. The futon stack is downstream
of this ethic: it builds the substrate where such refusals can be
RECORDED (proof paths), CITED (HEAD-as-escrow as 既), and AGGREGATED
(sorrys.edn as the 債 ledger).

### 2. PKD's methodological signatures are futon-theoretic instruments

| PKD's move | What it does | Futon-theoretic name |
|---|---|---|
| "I like to build universes which do fall apart" | Failure mode IS the data | combining-methods-as-diagnostic at cosmological scale; the disagreement IS the signal |
| "Fake fakes" (real birds for animatronic ones at Disneyland) | Configuration survives migration across real/fake regimes | Wyrd-salience in pure form |
| Refusing to explain the Flow My Tears coincidences | Inhabits 間; refuses "mere precognition" / "mere coincidence" | The "mere X" anti-pattern named by negative example |
| The Snakes of Hawaii / Unreal God codex jokes | Negative knowledge that persists structurally | Patterns whose 鹵 is absent but whose 皿 still composes a salient hole |
| Children as lie-detectors | 香 operating without 咅 | Pre-articulation perception as load-bearing |
| The German translator's "brand name" mistake | The catastrophic collapse of Logos | The central "mere X" move applied to articulation itself |

### 3. The Flow My Tears chain is a worked wyrd-example

The chain — novel (1970) → Kathy/Jack/inspector (1970) → gas-station
re-enactment (1978) → Acts/Daniel/Revelation (~50 AD) → Watergate
(1974) — is one configuration surviving translation across at least
six rival containment regimes:

| Regime | Container | Form of the configuration |
|---|---|---|
| PKD's unconscious (1970) | First-person dream-experience | A vivid dream demanding to be in the novel |
| Fiction (1970-74) | *Flow My Tears, the Policeman Said* | Felix Buckman meets a black stranger at an all-night gas station |
| Biographical life (1970) | Christmas Day meeting | Kathy/Jack/police-inspector triad matching the novel |
| Personal life (1978) | All-night gas station event | The fictional scene re-enacted with PKD as participant |
| Biblical text (~50 AD) | Acts; Daniel 7:9; Revelation 1:13-17 | Philip-meets-Ethiopian-eunuch; ancient-of-days; "I am the first and the last" |
| Political events (1974) | Watergate | "Warning to the powerful: you will shortly be judged" |

The configuration is wyrd-salient by every diagnostic in
futon-theory/wyrd. It survives translation. No single regime fully
contains it. PKD's refusal to collapse it — his four years of
unsuccessful theory-shopping — is the discipline futon theory names.
He inhabits 間 with theological seriousness; he refuses the available
"mere X" moves.

What he lacks in 1978: the vocabulary to say what the configuration
IS structurally. What futon theory adds: the name (wyrd-salience),
the criterion (survival across rival 部), the ledger (鹽/債), and the
operational discipline (combining-methods-as-diagnostic at agent-turn
granularity rather than only at decade-of-confessional-essay
granularity).

### 4. PKD's temporal vocabulary maps onto the three Norns

His theory-shopping (*"circular time, frozen time, timeless time,
sacred vs mundane time"*) is exactly the work futonic-logic §2.7 now
does formally:

- **既 (Urðr / earned-past):** A.D. 50 as load-bearing past underlying
  1978. *"a specific permanent landscape underlies the world of
  change... it is the period immediately following the death and
  resurrection of Christ."* The biblical-time as 既-surface beneath
  the 1978-time as Verðandi-surface.
- **化 (Verðandi / becoming-event):** 1978 as the presently-becoming,
  the "Disneyland" surface, what we see but cannot rely on as ground.
  He doesn't trust it but he lives in it.
- **應/債 (Skuld / owed-obligation):** *"Time is speeding up. And to
  what end? Maybe we were told that two thousand years ago."* The
  return-of-Christ is the debt the present incurs. The eschatological
  frame IS sorrys.edn at civilisational scale.

His Heraclitus citations are the same ones futon-theory/wyrd cites:
Fragment 54 (*"latent structure is master of obvious structure"*) and
Fragment 52 (*"time is a child at play, playing draughts"*). PKD
reads the same fragments through the same compositional lens.

His anamnesis-via-gold-fish-sign moment is, in our vocabulary, the
moment 既 became live to him — the 1970s present suddenly grounded
in a citeable ~50 AD past. The fish was 香 (embodied salience) firing.
"Loss of forgetfulness" is the operational definition of
retroactive-canonicalization at a personal scale.

### 5. The brand-name anti-pattern — the single most load-bearing passage

The German translator's rendering of Ubik's *"I am the word"* as
*"I am the brand name"* is the central futon-theoretic anti-pattern
in the essay.

Translating Logos (articulation that thinks itself, thinker and
thought together) into brand-name (articulation that sells itself,
separated from any thinker) is the "mere X" move applied to the
highest stratum of composition. It is what manufactured
pseudo-realities DO. The translator's innocent error is structurally
the same error as the cop-show's "the police always win" reduction,
the same error as Disneyland's naturalising of fake birds. PKD
identifies the error without naming it as the anti-pattern; futon
theory names it.

For the cold-open this is gold. A 3-move arc:

> *"I call it futonic discipline. Philip K. Dick called it 'How to
> Build a Universe That Doesn't Fall Apart Two Days Later.' Discipline,
> in his case, was building universes that DID fall apart — on purpose
> — so the failure modes became diagnostic. The opposite discipline
> is the German translator's: rendering 'I am the word' as 'I am the
> brand name,' and never noticing what was lost."*

Discipline → PKD's secret love of chaos → the anti-discipline
(brand-naming the Logos).

### 6. What futon theory adds, and why PKD is the right ancestor

PKD's essay is a manifesto without a substrate. He has:
- The diagnostic (reality = what survives belief-cessation)
- The ethic (refuse what should not be done)
- The case study (Flow My Tears chain)
- The anti-pattern (brand-naming Logos)
- The four-year refusal to collapse

What he doesn't have in 1978:
- A REGISTRY for the 債 ledger (he carries it in personal memory and
  confessional essay; the essay IS his sorrys.edn)
- A PROOF PATH so refusals become 既 for others
- A COMBINING-METHODS practice so diagnostics fire at agent-turn
  granularity, not at decade-of-essay granularity
- A SUBSTRATE where authentic-human-refusal is operationally cheap
  rather than heroically expensive

That last point is the futon stack's central design move. PKD wrote:
*"Their deeds may be small, and almost always unnoticed, unmarked by
history. Their names are not remembered."* The futon stack is built
so the small refusals don't have to be heroic — they get recorded as
proof paths, accumulate as 既, generate 應/債 obligations that compose
at scale. The authentic human's refusal becomes infrastructure rather
than martyrdom.

This is what makes Joe's "science fiction writer who writes in code"
framing tight, and not just clever. PKD wrote about authentic humans
refusing tyrannies; the futon stack is the substrate where refusal is
what the substrate DOES by default. Fiction becomes operative. Code
becomes wyrd-engineering. The crypto-speculator joke is structurally
correct: speculative fiction written in a language that executes is
a different kind of inhabited present.

### 7. The "I rediscovered futon theory in PKD" hypothesis

Joe's musing: *"For all I know, Dick already fully conceived of Futon
theory, and I only 'rediscovered' it by subconsciously reflecting on
his books and essays."*

Under the wyrd-survival framework, the hypothesis is the right shape
even if false in detail. The compositional structure futon theory
names is not an artifact of 2020s engineering culture. It is
recoverable from PKD's 1978 essay. It is recoverable from Heraclitus
DK B54. It is recoverable from the Norns. Wyrd-salience is itself the
diagnostic: if futon theory survives translation across a 1978 essay,
2500-year-old fragments, and Norse cosmology, it is not a local idiom
— it is what those configurations are pointing at.

The honest framing for the foreword is therefore not "Dick anticipated
futon theory" (a credit claim, wyrd-collapsing) but: *"Futon theory
is one operationalisation of a structural condition that surfaces
wherever Dick's two questions are asked seriously."* Dick is the most
lucid 20th-century articulator of those questions. That gives him the
place in the literature that this kind of essay-use requires.

### 8. Quotable passages for the foreword, with futon-theoretic gloss

| PKD quote | Foreword use | Futon gloss |
|---|---|---|
| "Reality is that which, when you stop believing in it, doesn't go away." | Cold-open companion to the title | Wyrd-survival criterion verbatim |
| "I like to build universes which do fall apart... I have a secret love of chaos." | Justifying verbose proof-path event protocol | Build for diagnostic failure; the 鹽/債 ledger keeps the failure usable |
| "Fake realities will create fake humans... fake humans will generate fake realities and then sell them to other humans." | Naming what the stack defends against | Brand-name anti-pattern at civilisational scale |
| "The basic tool for the manipulation of reality is the manipulation of words." | Justifying patterns-as-articulation discipline | 咅 is load-bearing; flexiarg patterns are typed articulations |
| "The authentic human being is one of us who instinctively knows what he should not do." | Defining who the stack is for | The 戒-practitioner; the Skuld-bearer who refuses |
| "Their deeds may be small, and almost always unnoticed, unmarked by history." | Justifying low-ceremony proof-path records | Small refusals become 既 by infrastructure, not by history |
| "Latent structure is master of obvious structure." (Heraclitus 54 via PKD) | Justifying combining-methods-as-diagnostic | The disagreement signal IS latent structure |
| "I do not know how much of my writing is true, or which parts (if any) are true." | Epistemic-posture model | `:prototyping-forward` vs `:technical-debt` typing as operational analogue |
| "I am the word" / "I am the brand name" | The collapse anti-pattern made visible | The "mere X" move applied to Logos itself — the central anti-pattern |
| "Time is speeding up. And to what end?" | Bridging to the Skuld frame | (應 . 債) projection at civilisational scale |
| "When time ends, the birds and hippos and lions and deer at Disneyland will no longer be simulations, and, for the first time, a real bird will sing." | Closing image | The Verðandi-surface becoming 既; the eschatological 鹽 |

## Projection surface 3: Candidate upgrades to futon theory

Each candidate below is a deferred futon-theory move surfaced by the
reading. They are not yet pattern files; they are hypotheses about
what would refine the library if developed. Status flags use the
mission's own `:kind` discipline (cf.
`feedback_prototyping_forward_vs_debt`): `:speculative` = idea
worth keeping but not yet load-bearing; `:prototyping-forward` =
plausibly the next move; `:dispatched` = handed to a follow-on
mission or absorbed into another pattern.

### Candidate 1: anamnesis-as-personal-canonicalization

`:prototyping-forward`. PKD's gold-fish-sign moment is canonicalization
at the level of a single agent's biography — the recovery of latent
prior salience into citeable 既 — but it is not the same as library
canonicalization (cf. `retroactive-canonicalization.flexiarg`).
Library canonicalization promotes a tension into a flexiarg via
NAMING → SELECTION → CANALIZATION. Personal canonicalization
promotes a forgotten configuration into present grounding via
ANAMNESIS (loss of forgetfulness). Both produce 既. The two may want
to be siblings in a "canonicalization-across-granularities" frame.
Plausibly a new flexiarg: `futon-theory/anamnesis-as-canonicalization`.

### Candidate 2: civilisational-scale 鹽/債 ledger

`:speculative`. The current 鹽/債 ledger (`futonic-logic.flexiarg`
§2.8) is grounded at proof-path and mission granularities. PKD's
Watergate reading — *"the most powerful man in the world... fell like
a flaming star into ruin and disgrace"* as the discharge of a debt
the present incurred — suggests the ledger may extend to political /
historical granularities. Risk: scope creep, drift away from
operational discipline. Mitigation: keep the civilisational framing
as illustration only unless a concrete operational use surfaces
(e.g. policy-debt tracking in agentic-coordination contexts).

### Candidate 3: brand-naming-the-Logos as a named flexiarg

`:prototyping-forward`. The brand-name passage is the cleanest
named-anti-pattern in the essay. Candidate names: `articulation-collapse`,
`brand-naming-the-logos`, `mere-x-on-articulation`. The pattern would
name: collapsing articulation-that-thinks-itself into
articulation-that-sells-itself; the *"mere X"* move applied to the
highest stratum of composition. It is a specialisation of the existing
"mere X" anti-pattern in `futon-theory/wyrd`. May not deserve its own
flexiarg; could be a section inside wyrd or futonic-logic. Decide
after seeing whether the foreword uses it heavily.

### Candidate 4: build-for-diagnostic-failure as a design discipline

`:prototyping-forward`. PKD's *"secret love of chaos"* and his
preference for building universes that fall apart is a design
discipline: build artifacts whose failure modes are themselves
diagnostic, rather than artifacts engineered to never fail. The
proof-path event protocol's verbose audit trail is one expression
of this. The combining-methods-as-diagnostic discipline (already
in the library via `feedback_combining_methods_as_diagnostic`) is
another. Worth considering whether these consolidate into a single
flexiarg: `futon-theory/build-for-diagnostic-failure` or
`futon-theory/universes-that-fall-apart`.

### Candidate 5: negative knowledge as wyrd-substrate

`:speculative`. PKD's *Snakes of Hawaii* and *Unreal God and the
Aspects of His Nonexistent Universe* jokes name a structurally
interesting move: claims that persist precisely because they have
no positive content. In the 鹽 frame, these are configurations where
鹵 (latent substrate) is absent but 皿 (stabilizing container) still
composes a salient hole. The stack may have analogues:
yet-to-be-implemented-but-typed interfaces, sorries.edn entries that
declare absence with structure, mission scope statements that name
what's NOT being done. Worth examining: is "structured absence" a
first-class compositional kind?

### Candidate 6: cross-linguistic-triangulation as wyrd-survival meta-test

`:prototyping-forward`. The salarium/鹽-債/Skuld move (validated by
Joe 2026-05-22) generalises to a meta-pattern: when the same
structural configuration appears in multiple unrelated linguistic
registers, the triangulation IS a wyrd-survival test passing. The
configuration is not an artifact of any one language's idioms.
Candidate flexiarg: `futon-theory/cross-linguistic-triangulation-as-test`,
or absorbed as a section in `wyrd.flexiarg`. Cross-linguistic
puns become *evidence* under this framing, not decoration. This
is the principle Joe's "Latin/Chinese/Old English" joke is
operationally relying on.

### Candidate 7: projection-surface as first-class artifact

`:speculative`. The mission itself models projection-surfaces: the
foreword is one, the mission file is another, candidate-upgrades
are a third. PKD's *"sacred vs mundane time"* phrase, his secret
love of chaos, and his refusal to collapse coincidences all
operationalise as multi-surface inhabitation. Worth examining whether
"projection surface" deserves to be vocabulary in `futonic-logic`
alongside 象/部/咅/鹽 — distinct from 部 (decomposition regime) in
that a projection surface IS a perspective on the same configuration
rather than a different articulation of it. Candidate: new vocabulary
entry, or extension of 部 to include the "view-of vs. decomposition-of"
distinction.

### Candidate 8: agentic coding as wyrd-engineering

`:prototyping-forward` (likely closer to substrate-2 / live-geometric-stack
than to a single flexiarg). Joe's foreword frames *"the advent of
agentic coding has allowed me to turn a bunch of ideas... into working
prototypes."* Read futon-theoretically: agentic coding compresses the
gap between speculative-fiction-as-text and speculative-fiction-as-
running-system. The atom-of-the-future framing becomes operationally
testable. Worth scoping into a concrete mission or excursion that
makes this claim verifiable — perhaps via diachronic study of which
of Joe's 20s-and-30s ideas became live, and which 化-event triggered
each conversion.

### Candidate 9: PKD essay as flexiarg-citable text

`:speculative`. Should `library/essays/pkd-1978-reading.flexiarg`
exist as a pattern? Verdict in conversation: probably not yet — the
reading is a USE of futon theory on a text, not a reusable pattern
in its own right. But if subsequent missions return to PKD or to
similar texts, a reading-pattern format may emerge. Worth keeping the
question open; do not promote prematurely.

## MAP-phase: Formal grounding via Parr & Friston (2018)

Added 2026-05-22 (evening). This section converts the IDENTIFY-phase
gap statement (futon theory has been practised and named but not
theorised) into a MAP-phase formal grounding. The mission stays in
perpetual-projection mode overall — MAP work continues alongside
the existing projection surfaces.

### Gap statement, sharpened by the Parr-Friston reading

> Futon theory is the engineering-timescale operationalisation of
> generalised free energy minimisation. The three-Norn projection
> (既/化/應-債) is its temporal decomposition: past as δ-collapsed
> observations, present as action selection, future as hidden state
> with priors over preferred outcomes. The 鹽/債 ledger is its
> path-integral accounting: worth (preference satisfaction) and
> debt (preference divergence) as positive and negative contributions
> to a Hamiltonian action. Patterns (flexiargs) are its
> priors-over-preferred-outcomes registry. Pattern canonicalisation
> is its Baldwin-mechanism. Self-guiding software is its
> Hamilton's-Principle implementation. Wyrd-salience is the
> survival diagnostic for which priors are load-bearing across
> rival containment regimes.
>
> The cosmological commitment that crosses all four PKD-Baudrillard-
> Ballard-Baldwin threads is the free energy principle itself: a
> property of any system that persists in a fluctuating environment.
> "Intelligence exists outside us" because generalised free energy
> minimisation is substrate-neutral. Birds, octopuses, agents, and
> the futon stack all participate in the same imperative.

### What Parr & Friston actually argue (compact)

Two free-energy functionals compared:

- **F(π)** — variational free energy. Functional of beliefs about
  states/policies; function of observations. Preferences over
  outcomes are absorbed into the prior over policies via the
  *expected* free energy G(π).
- **𝓕(π)** — generalised free energy. Functional of beliefs about
  states, policies, AND outcomes. Preferences become an explicit
  component of the generative model. Future outcomes are treated
  as hidden states with priors P(o_τ); past outcomes are
  δ-collapsed to observations.

Key equation: 𝓕(π) = Σ_{τ≤t} F(π,τ) + Σ_{τ>t} 𝒢(π,τ).
Generalised functional = past variational + future expected, **as a
path integral over time**. Active inference becomes Hamilton's
Principle of Stationary Action.

Future-causes-past mechanism: prior beliefs about future outcomes
propagate backward through the generative model via forward-and-
backward message passing, distorting beliefs about hidden states
at every time point reaching back to the present. The future,
treated as hidden state, *causes* the present — and the present
causes the inferred past. T-maze simulation (Figure 4-5) shows
this concretely: identical proximal behaviour, divergent distal
beliefs about where the agent expects to end up.

### Three-Norn ↔ generalised-free-energy mapping (exact, not metaphorical)

| Norn / glyph | Parr-Friston construct | Status |
|---|---|---|
| Urðr / 既 | τ ≤ t: past observations as Q(o_τ\|s_τ) = δ(o_τ) | Citeable; load-bearing; cannot be re-inferred |
| Verðandi / 化 | τ = t: present action — the policy π presently applied | Becoming-event; the loop turn firing |
| Skuld / 應⋅債 | τ > t: future hidden state with prior P(o_τ) over preferred outcomes | The "shall" baked into the generative model |

The 鹽/債 ledger maps onto Parr-Friston's risk-plus-ambiguity
decomposition of 𝒢(π,τ):

- **Risk** = D_KL[Q(o_τ\|π) ‖ P(o_τ)] = divergence from preferred
  outcomes. This IS 債 (debt) at proof-path granularity.
- **Ambiguity** = E_Q[H[P(o_τ\|s_τ)]] = states with uncertain
  observation-mapping. This is the 間 (honest-interval) at the
  level of inference — gaps in the likelihood matrix that resist
  premature filling.
- **Worth** (the 鹽 credit side) = preference satisfaction =
  −Risk minus epistemic foraging of low-ambiguity states.

The ledger-anti-collapse rule (futonic-logic §2.8 — "do not collapse
the ledger to net") was the right call without knowing why: net
is meaningless when the integrand is the operationally-relevant
quantity. Hamilton's action is the integrand-over-time, not its
net value.

### Sorries.edn as prior-over-outcomes registry (operationalisation)

A sorry is, formally, a prior over a preferred future outcome that
has not yet been realised. The `:kind` typing chooses *which*
prior:

- `:prototyping-forward` — confident prior over preferred future
  outcome ("this incomplete piece will be completed in a way that
  satisfies its declared shape")
- `:technical-debt` — less-confident or pessimistic prior ("this
  requires structural rework to satisfy any preferred outcome")
- `:n-a-by-design` (cf. M-war-machine-aif-last-mile §2.D) — prior
  that this channel is belief-independent by design

This means sorrys.edn is, in a very precise sense, **the futon
stack's prior-over-outcomes registry**. Every entry distorts belief
trajectories about current hidden states via the future-causes-past
mechanism. The M-war-machine-aif-last-mile sorries with their
v2 `:kind` schema are already operating this way — the Parr-Friston
formalism is the explanation, not a redesign.

### Baldwin as patron saint, finally explained

Baldwin's effect — behavioural plasticity canalises into inherited
genotype over generations — is the *biological instantiation* of
generalised-free-energy minimisation across evolutionary timescales.
Organisms minimise 𝓕 by acting consistently with priors over
preferred outcomes (survival, reproduction); behaviours that
consistently satisfy these priors become hidden states of the
genotype; the prior canalises into the model; across generations,
genotype-as-prior strengthens; the future (preferred survival
outcome) causes the past (canalised genotype).

This is *exactly* the operational pattern futon theory implements
at engineering timescales — agents minimise futon-theoretic free
energy by acting consistently with patterns; patterns that
consistently satisfy preferences canalise into invariants
(`structural-tension-as-observation`, `retroactive-canonicalization`);
across pattern-application generations, library-as-prior
strengthens; the future (preferred system properties) causes the
past (canalised library).

**Patron-saint claim, restated formally:** Baldwin = generalised
free energy minimisation at evolutionary timescale. Futon theory =
generalised free energy minimisation at engineering timescale
compressed by agentic coding. Same mechanism, different time-base.

### Independent triangulation: the npt/UKRN working paper

`~/npt/working-paper/UKRN_WP_draft_v10a.md` independently develops
the same construct in a non-Futon domain. Joe's UKRN-Services
simulation:

- §3 explicitly: *"Engagement at the institution side is a path
  integral"*; per-tick gating by three conditions (priority
  alignment, competitor gap, budget); cumulative engaged value as
  the integral.
- §3 four-term score per candidate decision: risk, ambiguity,
  pattern firing, priority alignment. Direct match for the
  `expected-free-energy-scorecard.flexiarg` decomposition (already
  in `library/aif/`, currently in expected — not generalised —
  formulation).
- §3 "Friston-style active inference; expected-free-energy
  decomposition; softmax sampling under a precision parameter;
  quarterly time anchoring" — explicit acknowledgement.

The triangulation matters: same structural pattern appears in
(a) Friston's theoretical neuroscience (generalised free energy,
path integral, future-causes-past), (b) Joe's UKRN simulation
applied to ORP institutional engagement (engagement-as-path-integral,
four-term EFE-scorecard), (c) the futon stack's agent-coordination
apparatus (sorries-as-priors, mission lifecycle as Hamiltonian
trajectory), (d) PKD's 1978 "future causes past" phenomenology
(Felix Buckman's prefigured gas station; A.D. 50 underlying 1978),
AND (e) Buddhist Magga as 2500-year-old eightfold-path practice
(see §"The Eightfold Path correspondence" below). Per
`feedback_cross_linguistic_triangulation` (the principle Joe
validated with the salarium/鹽-債/Skuld move): when the same
structure surfaces in this many independent registers, the
triangulation IS a wyrd-survival test passing. The structure is
not an artefact of any one domain.

### The Eightfold Path correspondence

Recognised (not engineered) 2026-05-22 evening after Joe's recent
addition of HEAD pushed the mission lifecycle to eight phases. The
mapping between Buddhist Magga (the Noble Eightfold Path) and the
mission lifecycle in `futon4/holes/mission-lifecycle.md` is one-to-one
and structurally tight:

| Eightfold Path | Mission phase | Why the correspondence holds |
|---|---|---|
| Right View (sammā-diṭṭhi) | **HEAD** | See the gap as it actually is before naming it. The honest interval (間) is sammā-diṭṭhi at operational granularity. |
| Right Intention (sammā-saṅkappa) | **IDENTIFY** | Name what you intend to close. Articulation (咅) = intention made explicit. |
| Right Speech (sammā-vācā) | **MAP** | Speak truthfully about what already exists; no exaggeration, no invention. Earned past (既) faithfully reported. |
| Right Action (sammā-kammanta) | **DERIVE** | The design IS the action — commit to a composition under a chosen regime. 鹽 = the act of generative composition. |
| Right Livelihood (sammā-ājīva) | **ARGUE** | Does this design EARN its place in the stack? Weakest correspondence; "livelihood" → "sustainable ethical work" → argument-as-elevator-pitch-plus-coherence-check. |
| Right Effort (sammā-vāyāma) | **VERIFY** | Sustain the wholesome (passing constraints), prevent the unwholesome (architectural breakage). Targeted spikes are precisely sammā-vāyāma. |
| Right Mindfulness (sammā-sati) | **INSTANTIATE** | Clear awareness during the becoming-event. *Sati* IS futon theory's 化 (Verðandi) read from inside the act. |
| Right Concentration (sammā-samādhi) | **DOCUMENT** | Sustained attention that lets the work crystallise into durable, navigable infrastructure. |

**Three-level correspondence.** The same shape recurs at one level
deeper: Joe's Operator's Foreword section headings (*Right Intention*,
*Right Speech*, *Right Mindfulness*) are explicitly Path items 2, 3,
and 7. So the triple correspondence is:

> Eightfold Path categories ↔ mission lifecycle phases ↔ foreword sections

None of the three were engineered to match the others. HEAD was added
to the lifecycle for operational reasons (operator-voice anchor before
formal scoping); the eightfold count fell out. The foreword's three
Path-titled sections were chosen for rhetorical reasons (motivation /
expression / refinement); their alignment with Path items 2/3/7 is
not aimed at.

**Why the recognition matters.** This is the wyrd-survival meta-test
in its purest form (cf. Candidate 6 in projection surface 3, and
`feedback_cross_linguistic_triangulation`): the same structural
shape recovered across genuinely independent registers without being
explicitly engineered into any of them. The fact that **HEAD was a
recent addition** sharpens the recognition — Joe wasn't optimising
for an eightfold count; the eighth phase was needed for engineering
reasons (the live operator-shape needed an intake layer before
IDENTIFY hardened the mission). The count fell out. Structure
showed up.

**The Dhamma as prior art for futonic modernism.** Joe's framing:
*"Buddha Dhamma feels quite modern, specifically because it is not
dogmatic."* This is structural, not aesthetic. The Dhamma's
ehipassiko ("come and see") empirical stance is exactly the discipline
futon theory practises:

- **Anti-dogmatic empiricism** = the refusal of "mere X" closure
  (cf. `library/futon-theory/wyrd.flexiarg` HOWEVER section). The
  Dhamma is offered as practice to be verified, not doctrine to be
  accepted. Futon theory takes the same stance toward its own
  patterns: they are *priors over preferred outcomes*, not assertions.
- **The Four Noble Truths as combining-methods-as-diagnostic.**
  Dukkha (suffering) is named; its cause is articulated; cessation
  is asserted; the path is given as a way to test the assertion. The
  structure is diagnostic-first — name the tension, then propose
  the path. Exactly the mission lifecycle's IDENTIFY-then-everything-
  else discipline.
- **Sati (mindfulness) as the operationalisation of 化.** Right
  Mindfulness IS the inside-view of the becoming-event. Futon theory's
  Verðandi-surface (M-live-geometric-stack) makes this externally
  observable; the Dhamma names what the practitioner needs to do
  internally during the same phase.
- **The path as path-integral.** The eightfold path is not eight
  separate practices but eight aspects of one integrated practice
  walked over time. This is structurally Hamilton's principle —
  the path through practice-space that minimises action (dukkha)
  is the path simultaneously satisfying all eight aspects.

The Dhamma is therefore not retroactively shoehorned into futon
theory. The recognition is the reverse: futon theory's discipline
turns out to be one engineering-timescale instantiation of a
2500-year-old practice tradition. The Dhamma is *modern* in the
specific sense Joe means — its anti-dogmatic empiricism makes it
structurally aligned with contemporary wyrd-modernism — and is
therefore one of the most lucid prior expressions of what the
futon stack tries to build.

**Operational consequence.** This expands the IDENTIFY-phase gap
statement: futon theory has been practised, named, and (via
Parr-Friston) given formal mechanism — and is now recognisable as
the engineering-timescale operationalisation of practice traditions
much older than 20th-century neuroscience. The four theoretical
anchors (PKD / Baudrillard / Ballard / Baldwin) gain a fifth: the
Dhamma. Unlike the first four, the Dhamma is not a theoretical
genealogy but a practice tradition. The mission's MAP work may
therefore want to engage Dhamma sources directly — Anguttara Nikāya
on right effort, Satipaṭṭhāna Sutta on mindfulness, the structural
treatment of the Path in Visuddhimagga — as MAP-phase material in
its own right.

### MAP-phase deliverables (concrete artefacts)

These are the work-items MAP-phase will produce. Each is a candidate
for a follow-on excursion or sibling mission rather than being
delivered in M-weird-modernism itself.

1. **Candidate flexiarg: `futon-theory/generalised-free-energy-as-substrate`**
   (or shorter: `futon-theory/futon-as-gfe`). Captures the
   three-Norn ↔ generalised-free-energy mapping at flexiarg
   granularity, with the path-integral accounting and the
   future-causes-past mechanism as load-bearing claims.
   Cross-references `library/aif/*` and `library/futon-theory/wyrd`.

2. **`library/aif/` audit excursion.** Sixteen patterns currently
   encode the expected-free-energy approach. Audit each for whether
   it should be updated to the generalised formulation:
   - `free-energy-as-tick-scalar` — F per tick. May not change; F
     is well-defined under both formulations.
   - `expected-free-energy-scorecard` — G decomposition. Generalised
     formulation makes preferences explicit in the model; the
     scorecard already does this in practice. Worth a sharpening
     pass to make the formulation explicit.
   - `belief-aware-risk-term`, `predictive-entropy-as-ambiguity` —
     specialisations of risk and ambiguity terms. Likely fine.
   - `structured-observation-vector`, `decomposed-prediction-noise`,
     `predictive-coding-belief-update` — substrate. Likely fine.
   - `policy-precision-commitment-temperature`,
     `hierarchical-budget-aware-action-selection` — policy-side.
     May need re-grounding once preferences-as-priors is the
     central frame.
   - `experimental-comparison-of-EFE-variants` — directly relevant;
     a Parr-Friston variant should join the comparison set.
   The audit is excursion-scale, not mission-scale.

3. **M-live-geometric-stack ↔ generalised free energy mapping
   excursion.** Hypothesis to test: substrate-2's `(T, ∇, Δ, drift)`
   geometric quantities map onto generalised-free-energy components.
   - T = residual generalised free energy at a region?
   - ∇ = variational derivative of 𝓕 (the update-equation gradient)?
   - Δ = Hamilton's action increment per tick?
   - drift = the optimistic distortion of future belief
     trajectories (cf. Sharot et al. 2012, cited in P&F)?
   These are research hypotheses requiring engagement with the
   substrate-2 codebase, not assertions. Worth an excursion.

4. **M-war-machine-aif-last-mile reading.** The WM AIF apparatus
   is operationally implementing the Parr-Friston structure already:
   sorrys.edn `:kind` schema = priors over outcomes; per-channel R3
   likelihoods = the A matrix Cat(A); EFE-scorecard = G decomposition;
   R10 scheduled execution = the temporal cadence the path integral
   integrates over. The mission's R-criteria contract is a worked
   instance of generalised-free-energy operationalised. Worth a
   reading-pass that makes this explicit — both for the WM mission's
   own theoretical clarity and for transferring the pattern to
   other AIF-instantiating subsystems.

5. **Foreword footnote (or annex pointer).** Joe's foreword line
   *"the software system can be largely self-guiding, noticing its
   own points of tension, and repairing them"* now has a formal
   citation route: Parr & Friston (2018), via generalised-free-energy
   minimisation operating on a library-as-prior-registry. If the
   foreword wants a technical-reader ground-out, a single footnote
   pointing to this mission's MAP section will do it.

### Hamilton's Principle of Stationary Action — bonus structural claim

𝓕(π) is a Hamiltonian Action. The futon stack's path through
belief space (proof paths, mission progressions, library evolution)
is *physically lawful* in the same sense classical mechanics is —
it follows the path that minimises action. This is the strongest
theoretical grounding the stack has yet had, and it is recoverable
from the Parr-Friston path-integral formulation.

The "atoms of the future that exist in the present" framing in
Joe's foreword has a precise reading under this lens: these atoms
are the *boundary conditions* on the path integral. They constrain
the trajectory without specifying it — exactly as a Lagrangian's
endpoints constrain a path without dictating it. The futon stack
identifies and develops atoms-of-the-future by registering them
as priors (patterns, sorries, invariants); the path integral does
the rest.

## Open threads

- The "I rediscovered futon theory in PKD" speculation needs a
  durable framing for the foreword that doesn't sound like credit-
  claiming. The current draft handles this with *"I suspect that I
  only 'rediscovered' futon theory while subconsciously reflecting
  on his work"* — modest and wyrd-honest. Candidate sharpening:
  follow with a sentence that converts the personal speculation into
  a structural claim ("the structure surfaces wherever Dick's two
  questions are asked seriously"). Optional; leave open.
- Should the foreword include the brand-name → Logos passage
  explicitly, or keep the 3-move arc compressed to discipline / PKD-
  title / love-of-chaos? Decision deferred to Joe's next pass on the
  draft.
- The "Rome remains Rome" line invites a direct citation of PKD's
  *"if we are really living in the Roman Empire, somewhere in Syria,
  why do we see the United States?"* — but the foreword may be
  saturated with PKD already; over-citation could undercut the move.
  Defer to drafting taste.
- The mission's own status: should it ever leave IDENTIFY? Possibly
  not. Per the wyrd-modernism framing, the mission's load-bearing work
  is in the perpetual rotation between projection surfaces. A
  conventional "close" might be wyrd-collapsing. Worth holding the
  question open and revisiting if/when concrete library upgrades
  ship from this mission's threads.
- Candidate 3 (brand-naming-the-Logos) and Candidate 4
  (build-for-diagnostic-failure) are the most likely near-term
  promotions to flexiarg form. Candidates 1, 6, and 8 are
  medium-term. Candidates 2, 5, 7, 9 are deferred / speculative.

### Pending cleanup (deferred from this mission's ARGUE round, 2026-05-22 evening)

- **Verbose summary extraction in `mission_control_backend/mission-summary`.**
  When a mission's body starts with markdown front-matter (`**Status:**`,
  `**Owner:**`, `**Cross-refs:**` ...), the heuristic falls through to
  `post-status-paragraph` and slurps the entire front-matter block as
  the "summary" — multi-kilobyte text instead of a one-paragraph preview.
  M-weird-modernism's substrate-2 hyperedge currently carries ~5KB of
  this. Functional (downstream consumers can truncate) but excessive.
  Root cause: `extract-first-paragraph`'s `drop-while` doesn't recognize
  `**Bold-key:** value` lines as front-matter. Cheap fix: extend the
  `drop-while` predicate to skip lines matching `^\*\*[^*]+\*\*:?\s`.
  Not blocking; defer to a small follow-up pass.

## Mission log

- 2026-05-22 (morning) — IDENTIFY. Wyrd pattern landed
  (`library/futon-theory/wyrd.flexiarg`); futonic-logic three-Norn
  extension landed (§1, §2.7, §2.8, §3.1, §4.12-4.14, §6).
  Futon-theoretic reading of PKD 1978 produced in conversation,
  filed here as projection surface 2 / HEAD. Operator's Foreword
  intro draft filed as projection surface 1. Candidate upgrades to
  futon theory enumerated as projection surface 3. Mission opens
  in perpetual-projection mode; no closure phase scheduled.
- 2026-05-22 (afternoon) — IDENTIFY-phase gap statement: futon theory
  has been practised and named but not theorised. Four theoretical
  anchors named: PKD as primary source, Baudrillard's precession
  inverted, writer↔theorist dialectic reversed, Baldwin as patron
  saint. Cosmological commitment: intelligence exists outside us.
- 2026-05-22 (evening) — MAP-phase grounding via Parr & Friston (2018)
  added. Three-Norn ↔ generalised-free-energy mapping locked in
  (既=δ-collapsed past, 化=present action, 應⋅債=future hidden state
  with priors). 鹽/債 ledger mapped to risk-plus-ambiguity
  decomposition. Sorrys.edn identified as prior-over-outcomes
  registry; `:kind` typing identified as choice of P(o_τ). Baldwin
  patron-saint claim formalised as generalised-free-energy
  minimisation at evolutionary timescale; futon theory = same at
  engineering timescale compressed by agentic coding. Hamilton's
  Principle of Stationary Action identified as the deepest
  structural grounding the stack has had. NPT/UKRN working paper
  noted as independent triangulation in non-Futon domain
  (engagement-as-path-integral; four-term EFE-scorecard already
  operational). Five MAP-phase deliverables enumerated, each
  excursion-scale or smaller. Mission stays perpetual-projection.
- 2026-05-22 (late evening) — Eightfold Path correspondence recognised
  while drafting Right-Mindfulness bullets for the Operator's
  Foreword. With HEAD recently added, the lifecycle has eight phases
  that map one-to-one onto sammā-diṭṭhi / saṅkappa / vācā / kammanta /
  ājīva / vāyāma / sati / samādhi. The foreword's three section
  headings (Right Intention / Right Speech / Right Mindfulness) are
  Path items 2/3/7, giving a triple correspondence (Path ↔ lifecycle
  ↔ foreword sections), none of which were engineered to match.
  Buddhist Dhamma added to the Independent-triangulation register as
  the fifth domain in which the futon-theoretic shape surfaces.
  Joe's framing — *"Buddha Dhamma feels quite modern, specifically
  because it is not dogmatic"* — folded into MAP as prior art:
  ehipassiko empiricism IS the combining-methods-as-diagnostic /
  anti-"mere X" discipline futon theory practises. Suggested
  follow-on MAP material: direct engagement with Aṅguttara Nikāya,
  Satipaṭṭhāna Sutta, Visuddhimagga.

- 2026-05-23 — ARGUE-phase round on the WebArxana mission interface,
  pivoted from Rob's "still cluttered" felt-need report. The
  operational test the round names: **pratītyasamutpāda as the
  operational refusal of svabhāva at the mission-graph level**.
  *"One bare hyperedge isn't much to go on"* (Joe, this round): the
  futon stack could not investigate its own mission structure because
  mission entities were single opaque hyperedges with no edges to
  their conditions of arising. The `@factor Keen investigation
  (dhammavicaya)` label across the flexiarg library was promissory
  until dependency structure became inspectable. The round closed
  the most acute gap and parked the deeper structural-ingest work
  on a clean substrate.

  **What landed:**

  - **Single-parser consolidation** (Task 8). Three mission parsers
    were running concurrently; consolidated down to one.
    `futon3c.peripheral.mission-control-backend/parse-mission-md` is
    now the canonical parser — extracts `:mission/summary`,
    `:mission/cross-refs`, `:mission/code-paths`, `:mission/phase`
    in addition to the prior id/title/status/repo/date/owner/raw-status
    fields. `file_ingest.clj/ingest-mission-doc!` threads the enriched
    record into substrate-2 hyperedge props. `build-inventory`
    refactored to read substrate-2 (not filesystem). `futon3a/src/
    futon/missions.clj` refactored to a thin substrate-2 reader —
    `parse-mission-doc` deleted, filesystem-reparse enrichment in
    `hyperedge-props->record` deleted, filesystem-walk fallback in
    `load-missions` deleted. Dead-code purge: 892 → 682 lines
    (-210 lines). Backfilled substrate-2 across the 7 default-repo-
    roots: 126 missions re-ingested with enriched props.
    `/api/alpha/missions` count went 139 → 197 (substrate-2 covers
    more repos than the previous filesystem default set).
  - **`arxana://view/missions-portfolio` un-blanked** (Task 9). The
    Emacs view was pointing `arxana-evidence-server` (port 7071,
    futon1a) at a path futon1a doesn't serve, producing a
    double-`/api/alpha/` URL that returned nothing. Added a
    dedicated `arxana-mission-control-server` defcustom in
    `arxana-browser-missions.el` defaulting to `http://localhost:7070`
    (futon3c's mission-control endpoint). The view now renders.
  - **Code-paths regex tightened** as a small follow-on fix. Post-
    filter strips trailing sentence punctuation and requires a
    real-looking extension on the last segment. M-weird-modernism's
    captured paths went 18 → 9 (clean).
  - **JVM rationalisation, round 1** (Task 10). Killed shadow-cljs
    watcher (PID 1588664) — a dev-time CLJS rebuilder, not in the
    request path. Compiled `main.js` keeps being served by futon3c.
    3 JVMs → 2 JVMs. Task 11 (War Machine consolidation: 2 → 1)
    scaffolded as a follow-on task with the full algorithm in the
    description — not a new mission, per the methodological
    correction below.

  **Methodological finding** (saved as
  `feedback_missions_vs_tasks`): **missions are for unknown
  specifications; tasks are for known algorithms however complex.**
  Joe corrected mid-round when I proposed scaffolding a new mission
  for the WM consolidation: the algorithm was already written, so
  it's a task, not a mission. Size and known-vs-unknown-spec are
  different axes. Saved as feedback so future rounds don't ceremony-
  inflate sizeable-but-specified work into mission files.

  **Pending from this round** (as concrete tasks, not new mission
  scope):

  - **Task 2** — sketch the centralised edge-type registry.
  - **Tasks 3 / 4 / 5** — PSR / PUR / Excursion ingest (the original
    dependency-structure ambition that started this round). The
    single-parser consolidation cleared the substrate so this work
    can now land cleanly on a known-good base.
  - **Task 6** — verification: confirm the Graph view shows the
    dependent-arising structure once 3 / 4 / 5 land. The
    pratītyasamutpāda test surfaces here — orphan PSRs without
    PURs, PURs against retired patterns, excursions referenced by
    completed missions become visible findings.
  - **Task 11** — War Machine consolidation. Algorithm specified;
    substantial work; lands JVM count at 1.

  **Verbose-summary cleanup** noted in this mission's Open threads
  as a small follow-up (cheap fix; defer).

  **Mission stays IDENTIFY + MAP, perpetual-projection.** The
  ARGUE-phase increment is captured here; the projection-surface
  framing of the mission accommodates further ARGUE rounds when
  Tasks 2-5 and 11 land.
