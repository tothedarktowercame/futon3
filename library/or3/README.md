# or3 — patterns extracted from the Brookes Open Research case studies

Seventeen design patterns distilled from seven interview-based case studies of
open research practice at Oxford Brookes (`or-case-study-template-draft-v4.org`).

Where `or/` covers the mechanics of openness (licences, DOIs, platforms,
identifiers) and `or2/` covers the institutional topology (mandate, stewardship,
horizon roles), `or3/` covers **transferable practice** — what these particular
people did that a colleague in another School could adopt.

## The anonymisation policy

The case studies are about named individuals. The patterns are not.

- **The pattern body is anonymous.** IF / HOWEVER / THEN / BECAUSE describe a
  generic situation and a generic move. No individual is named, and nothing
  depends on knowing whose project it came from.
- **NEXT-STEPS may name a local contact**, because routing someone to a
  colleague who has done the thing is the most useful step a pattern can offer.
  Contacts are given as team-and-School rather than personal contact details.

Anyone named in a NEXT-STEPS entry should be asked before this library is
circulated — the case studies were given for a case-study collection, and
being listed as a source of advice is a further ask.

## The extraction rule

**Extract the method, not the apparatus.** The point is not "build your own
simulation suite" — that would duplicate capital investment for no gain. It is
"teach the process rather than the facility", which is the part that transfers.
Each pattern was written by asking what a reader *without* the original
project's resources could actually do on Monday.

## Provenance

| Pattern | Source case study |
|---|---|
| `sustainability-without-enclosure` | 1 — VSAT (open-source toolkit, PoC commercialisation) |
| `reproducibility-as-a-teaching-habit`, `borrow-a-training-network` | 2 — Literate Programming in R |
| `skills-that-travel-onward`, `plural-value-accounting` | 3 — Open Value Networks in craft practice |
| `co-design-is-not-consultation`, `design-around-the-red-line` | 4 — First Steps (post-diagnosis peer-led programme) |
| `co-design-the-call`, `count-every-card-back`, `consent-bounded-analysis` | 5 — AD[A]PT (co-designed doctoral programme) |
| `teach-the-process-not-the-facility`, `scenarios-from-lived-experience`, `authorship-for-first-time-contributors`, `student-project-as-collaboration` | 6 — SimLab (simulation-based curriculum) |
| `coordinator-in-your-school`, `ask-first-then-bring-the-expert`, `openness-without-exposure` | 7 — Being an Open Research Coordinator |

## The hidden layer: scenarios in the Open Research Book

The book (`orbook.github.io`, `sources` branch) presents these patterns to
readers as **scenarios** — plain narrative paragraphs, no IF/HOWEVER/THEN/BECAUSE
apparatus and no mention of patterns at all. The word is borrowed from
simulation-based education, where a scenario is a situation you are placed into
in order to practise. The pattern layer stays here, underneath; readers meet only
the situation.

The mapping is one-to-one, and the scenario numbering is fixed by the table in
`About/Introduction.md`:

| # | Scenario (book) | Pattern (here) |
|---|---|---|
| 1 | Consulted, not co-designing | `co-design-is-not-consultation` |
| 2 | The control arm nobody would accept | `design-around-the-red-line` |
| 3 | Who decides what gets studied | `co-design-the-call` |
| 4 | Whose idea was that? | `count-every-card-back` |
| 5 | The transcripts and the chatbot | `consent-bounded-analysis` |
| 6 | Everyone wants a turn in the suite | `teach-the-process-not-the-facility` |
| 7 | The mannequin that taught nothing | `scenarios-from-lived-experience` |
| 8 | The actor who was never an author | `authorship-for-first-time-contributors` |
| 9 | The dissertation nobody opened again | `student-project-as-collaboration` |
| 10 | Open research is somebody else's job | `coordinator-in-your-school` |
| 11 | The seminar the sceptics skipped | `ask-first-then-bring-the-expert` |
| 12 | We can't publish our transcripts | `openness-without-exposure` |
| 13 | The analysis that could not be re-run | `reproducibility-as-a-teaching-habit` |
| 14 | Good materials, no budget | `borrow-a-training-network` |
| 15 | Sustain it, but don't close it | `sustainability-without-enclosure` |
| 16 | After the funding ends | `skills-that-travel-onward` |
| 17 | The costs the price does not show | `plural-value-accounting` |

Scenarios can also be spliced into individual chapters using the book's existing
`\note{}` "Practice Example" box. Scenario 5 is spliced into `Knowledge/Open_Data.md`
as a worked demonstration. Note the constraint: a `\note{}` body reaches LaTeX as
raw text, so keep spliced scenarios to plain prose — no Markdown links, and no
`_ & # %`.

The NEXT-STEPS contacts in the patterns below are **not** carried into the book;
they are internal routing.

## Status

Draft. Not yet reviewed by the interviewees, and not yet checked against
`or/` and `or2/` for overlap beyond the `@see` links each pattern carries.
