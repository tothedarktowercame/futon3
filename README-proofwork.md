# Flexiformal Proofwork Deliverables

This document covers the transport contract, pattern canon, check DSL, trail instrumentation, and workday bridge.
For the overview and quickstart, see `README.md`.

The full prototype roadmap lives in `holes/futon3.devmap` (it continues beyond Prototype 5); the sections below highlight the evidence already landed.

## Transport Contract & Golden Transcripts

Prototype 0 in `holes/futon3.devmap` now has a frozen check-job transport contract plus a sandbox profile for deterministic replays. The specs in:
- `docs/protocol/transport-contract-v1.md`
- `docs/protocol/golden-transcripts.md`
- `docs/sandbox/README.md`

...explain what MUSN check jobs expect on the wire, how the NDJSON transcripts prove request -> reply behavior, and how to run the sandboxed Clojure runner so those transcripts stay byte-for-byte reproducible.

## Pattern Canon & Similarity Field

Prototype 1 + Prototype 2 in `holes/futon3.devmap` describe the canonical pattern store, sigil metadata, and fake embedding that turn FUTON3 into the "pattern library of record."

- The library already lives under `library/` (e.g., the devmap-coherence IFR set + prototype-alignment checks) and comes with sigil indices in `resources/sigils/{index.edn,patterns-index.tsv}` plus density visualisations.
- `scripts/pattern_index.py`, `scripts/reembed_devmap_tfidf.py`, and the `:sigils` alias wire those EDN/CSV files into TF-IDF + sigil matrices so DEV/CI can rebuild the fake embedding whenever new flexiargs land. This is the evidence for "pattern canon with embeddings," not just prose.
- The "classical" helper exposed in Emacs (`my-futon3-embed-english`) reads the very same `resources/sigils/patterns-index.tsv` that the TF-IDF job emits: the job harvests hotwords + section tags from devmaps, while the helper consumes them to score free text against the catalog and surface the nearest clause (now echoed in the Tatami HUD). In other words, the hotword scan and TF-IDF build are two views over one dataset, not competing embeddings.
- `library/devmap-coherence/README.md` captures the current canon map (mojo/or/p4ng/transition/t4r) and lists the IFR/prototype families that keep the devmaps honest.

### Canonical Store

The canonical store now lives in Futon1 (XTDB) and the filesystem is treated as a checked-out working copy:
- Run `scripts/pattern_sync.clj` to push library edits into Futon1 before relying on the API
- Run `scripts/pattern_pull.clj` to pull Futon1 changes back into files
- `resources/pattern_store.edn` is a legacy cache only
- The shared library-coherence patterns (e.g., `library/library-coherence/*.flexiarg`) describe the meta checks that keep every source synced.

## Check DSL & Devmap Coherence Runs

Prototype 3 in `holes/futon3.devmap` + the `ifr-f3-piti.flexiarg` clause spell out the `check!` interface (pattern id, context EDN, evidence refs -> proof state + follow-on obligations). `library/devmap-coherence/*.flexiarg` and the shared `library/library-coherence/*.flexiarg` meta patterns are the actual DSL-ready specs, not hypotheticals.

`holes/AGENTS.md` turns that intent into a concrete plan: obligation extraction from the devmaps, sigil-driven candidate narrowing, `check!` evaluation, and proof exports back into FUTON1/FUTON2. This document is the evidence that the check DSL is wired into the daily "devmap coherence" cadence.

### CYOA Devmap Refinement

A CYOA runner now drives per-pattern interviews, Evidence Engineer prompts, and deterministic patching for `holes/futon*.devmap` entries:
- See `scripts/cyoa_room.clj` and logs under `holes/logs/cyoa`
- Skip rules in `resources/cyoa/skip_rules.edn` prevent re-running evidence generation when a block already has `+ next-evidence:`
- Pattern Critic runs as a separate turn, emits inline `@question:` prompts for ambiguous clauses, and can apply them directly to the devmap (`--apply-critic`)
- Flexiarg mode highlights `@question:` lines with warning face to keep clarifications visible during edits

## Trail Instrumentation & Proof-State Journal

Prototype 4 in `holes/futon3.devmap` plus the IFR patterns (e.g., `library/devmap-coherence/ifr-f5-samadhi.flexiarg`) demand that proof trails record `:pattern/id`, `:obligation/id`, tags, and joy deltas.

These instructions are backed by:
- `resources/tatami-events.edn` - current session-trail fixtures used by Tatami/Tatami-monitor
- `resources/tatami-context.edn` - rolling log of every `FROM-CHATGPT-EDN` HUD payload that the Emacs integration collects
- `resources/type_vocab.txt` and `resources/sigils/compressions.edn` - vocabulary that feeds those traces, ensuring we tag trail actions with the same ontology as the pattern canon

AIF episode summaries can now be attached as `:aif-trace` evidence so Futon2 experiments show up inside proof checks without hauling full traces into Futon3. See `docs/aif-trace.md` for the reasoning, the data shape, and how the bridge computes it.

## Workday Instrumentation & ChatGPT/Tatami Bridge

Prototype 5 in `holes/futon3.devmap` calls for a `workday/submit` endpoint plus bridges into tooling.

### Emacs Integration

The Emacs integration that already exercises this flow now lives in this repo as `contrib/aob-chatgpt.el` (mirrors the futon1 version) so FUTON3 can claim and evolve it alongside the transport. Prompt text files such as `contrib/futon0-clock-out.prompt` now sit beside the helper (override `my-futon-prompt-directory` if you keep prompts elsewhere).

The helper ingests prompts, spins up Tatami (`tatami.el`), and streams the pattern/type vocab coming from `resources/sigils/patterns-index.tsv` + `resources/type_vocab.txt` into classical embedding heuristics.

These assets, together with Tatami shell scripts under `../futon1/contrib/` (referenced in the file), prove that the workday bridge is operational: ChatGPT/Tatami sessions emit NDJSON trails, get scored via the fake embedding, and will become first-class API calls once the `workday/submit` surface in transport lands.

### Clock-Out Flow

Clocking out now runs automatically when you close a Futon chat buffer:
1. The helper inserts `contrib/futon0-clock-out.prompt`
2. Captures the EDN summary
3. When FOCUS headings are present in your Org agenda, surfaces a small "d = mark done, s = skip" mini-game so you can confirm AI-suggested completions before they propagate into FUTON1/FUTON3

Reopen the review via `M-x my-chatgpt-shell-open-last-focus-review` if you dismiss it; each run is also logged into FUTON1 as a `:clock-out/summary` entity with an efficacy score derived from your mini-game responses.

### Boundary Scans

Boundary scans now track the "negative space" around every futon layer:
- Run `scripts/devmap_readiness.py` to lint `holes/futon*.devmap` files and write `resources/boundary.edn` with last-touch timestamps, TODO counts, and evidence gaps for F0-F7
- This snapshot, together with the git vitality summary and Futon0's scanner output, now feeds the Stack HUD block inside the Tatami context so readiness drift (e.g., "f4 missing evidence for 13 prototypes") appears alongside the Tai Chi/curfew reminders
- Load `scripts/boundary_hud.el` in Emacs and run `M-x futon-boundary-hud` to see a Magit-style table for F0-F7: each row shows the orb (based on missing evidence ratio), last devmap touch, and outstanding TODOs
- Need a copy/paste briefing for agents? Run `python3 scripts/futon_summary.py futon0` (or any `fX`) to print the devmap's `@state` block plus the latest boundary entry from `resources/boundary.edn`

## Workday Submit & `check!` Handlers

- `type:"workday"` envelopes (WS or `POST /musn/ingest`) accept `{activity, evidence?, sigils?, prototypes?}` payloads, normalize them, and append to `futon3/logs/workday.edn`. Replies include `run-id`, `workday/id`, and `blocked-by [:f1.persistence]` to keep the dependency on Futon1 explicit until the adapter lands. The helper also exposes `futon3.workday/set-log-path!` for tests.
- `type:"check"` envelopes call the new `futon3.checks/check!` DSL. It loads `resources/sigils/patterns-index.tsv`, matches hotwords against the supplied `context` + `evidence`, logs proof states under `futon3/logs/checks.edn`, and returns `{status, missing, derived/tasks, proof}` so downstream futons can route the result. `check!` is also exposed to the SAFE REPL via `musn.api` bindings.
- Workday submissions can embed `{:check {:pattern/id ...}}` (or just `:pattern/id`), and the router will automatically run `check!` with the submitted activity/evidence before replying. This ties Prototype 3 + Prototype 5 together: instrumentation yields immediate applicability verdicts and proof trail lines even before FUTON1 persistence is wired in.
- Set `FUTON1_API_BASE` (e.g. `http://localhost:8080/api/alpha`) and `FUTON1_PROFILE` (e.g. `testing`) before `make dev` if you want Futon3 to mirror every workday/check result into Futon1 automatically. When these env vars are present the transport posts each workday/check to Futon1 so demo runs appear under that profile without extra scripts.

## Learn-or-Act Retrieval Loop

- `/musn/hints` now returns the learn-or-act envelope instead of bare pattern scores. Every response carries `:search/best-score`, `:search/candidates`, `:search/attempts`, `:failure/mode`, `:stub/current`, and `:gap/current` so Tatami events and dual-channel logs can persist the reasoning trace verbatim.
- Failure causes are explicit enums: `:failure/retrieve` (triage or micro-question needed), `:failure/represent` (insufficient EDN framing), and `:failure/gap` (library missing a fit). Success cases leave `:failure/mode` unset.
- True gaps emit two structures: a gap artifact persisted to `resources/futon1-gaps.edn` (uuid, timestamp, short context, why-codes, nearest known IDs, proposed title) and a provisional stub with only context/problem/first-moves/success bullets. Futon3 always tells the user when Gap Mode activates before riffing on the stub.
- Triaged-but-ambiguous searches surface as `:failure/retrieve` with a single-choice micro question ("Is this closer to A/B/C?") so the caller can tighten the query instead of hallucinating.
