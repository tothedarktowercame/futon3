# Futon3 MUSN Sandbox

A modern Clojure rewrite of the vintage Monster Mountain / MUSN server. The transport provides a battle-tested ingest path plus an **optional** multi-user REPL (SAFE via SCI, ADMIN via tokenized unsafe eval) while we wait to slot in an external persistence layer. Futon3 is also the canonical interface layer: shared ChatGPT/tatami shims (e.g. `contrib/aob-chatgpt.el`), pattern libraries, and sigil tooling live here so every other futon, including Futon1 storage, depends on a single point of integration.

## Prerequisites
- JDK 11+ and `clojure` CLI tools (`brew install clojure/tools-deps` or similar)
- Network access the first time you run `make test`/`make dev` so dependencies can be cached into the repo-local `.m2/` directory

## Layout Highlights
- `src/f2/transport.clj` – WebSocket/HTTP bus, async back-pressure, optional REPL wiring
- `src/f2/repl.clj` – SAFE (SCI sandbox) + ADMIN (token-gated) evaluators
- `src/f2/ui.clj` – JSON façade (`/musn/clients`, `/musn/sessions`, `/musn/practices`, `/musn/export`)
- `src/f2/musn.clj` – system lifecycle + demo ingest hooks
- `src/f2/semantics.clj` – placeholder reasoning over transport history
- `dev/demo_events.ndjson` – sample ingest for `make demo`

## Quick Start
1. Install deps (first run populates `.m2/`):
   ```bash
   make test
   ```
2. Boot the stack:
   ```bash
   make dev
   ```
   - Transport (WS+HTTP) → `http://localhost:5050`
   - UI endpoints → `http://localhost:6060`
3. (Optional) Load sample events:
   ```bash
   make demo
   # streams dev/demo_events.ndjson via the ingest path
   ```

## Elisp Package Tests
Use the package runner to target individual Elisp test files:
```bash
./scripts/test-elisp-packages.sh embedding
./scripts/test-elisp-packages.sh hud sessions
PACKAGE=bridge ./scripts/test-elisp-packages.sh
```
Supported packages: `embedding`, `hud`, `sessions`, `arxana`, `bridge`, `aob`, `all`.
Use `ERT_SELECTOR` to narrow tests, e.g. `ERT_SELECTOR='futon3-hud-*'`.

## Devmap PDF (A3)
We keep the seven futon devmaps in `artifacts/devmaps.tex`. To regenerate the emoji-rich A3 PDF (`artifacts/devmaps.pdf`):

1. Make sure TeX Live (or similar) provides `lualatex` and `Noto Color Emoji`.
2. From the repo root run:
   ```bash
   scripts/build_devmaps.sh
   ```
   The script regenerates `holes/artifacts/devmaps.tex` via `scripts/make_devmaps_tex.py`, then runs lualatex inside `holes/`. It keeps TeX caches local (`holes/luatex-cache` + `holes/.texlive2025/texmf-var`). Override `TEXMFCACHE`, `TEXMFVAR`, or `DEVMAPS_HOME` to point elsewhere if you already maintain a shared cache.

The helper just wraps:

```bash
(cd holes && \\
  TEXMFCACHE=. TEXMFVAR=$PWD/.texlive2025/texmf-var \\
  HOME=$PWD lualatex -interaction=nonstopmode -halt-on-error \\
  -output-directory artifacts artifacts/devmaps.tex)
```

…so you can also run that manually when experimenting.

## WebSocket Walkthrough
```
wscat -c ws://localhost:5050/musn/ws
> {"type":"hello","client":"alice","caps":["eval","message"]}
< {"ok":true,"type":"ack","rev":1,"run-id":"RUN-af03f1e8","client":"C-5a2b7c1d"}
> {"type":"eval","payload":{"code":"(+ 1 2)"}}
< {"ok":false,"type":"eval","err":"repl-disabled"}
```

### Enabling the REPL
The REPL is **OFF** by default. Start the system with a config override to expose SAFE mode (SCI sandbox, allowlisted namespaces, 64 KB code / 1 MB result caps, 100 ms CPU budget):

```clojure
(require '[f2.musn :as musn])
(musn/stop!)
(def system (musn/start! {:repl {:mode :safe}}))
```

Now evals succeed:
```
> {"type":"eval","payload":{"code":"(mapv :id (musn.api/clients))"}}
< {"ok":true,"type":"eval","mode":"safe","run-id":"RUN-af03f1e8","result":["C-5a2b7c1d"]}
```

SAFE mode exposes a tiny DSL under `musn.api/*` (currently `clients`, `history`, `links`, `now`) plus a curated subset of `clojure.core`/`clojure.set`.

To unlock ADMIN mode (arbitrary `load-string`), provide a token and restrict it to local addresses:

```clojure
(musn/stop!)
(def system (musn/start! {:repl {:mode :admin
                                :admin-token "top-secret"
                                :admin-allow #{"127.0.0.1" "::1"}}}))
```

```
> {"type":"eval","payload":{"code":"(System/getProperty \"user.dir\")","mode":"admin","token":"top-secret"}}
< {"ok":true,"type":"eval","mode":"admin","run-id":"RUN-d7b0e995","result":"/home/joe/code/futon3"}
```

### HTTP Ingest
Eval frames are WebSocket-only (we need the client identity + remote IP for gating). Other frames can be posted as NDJSON:
```bash
curl -X POST http://localhost:5050/musn/ingest \
  --data '{"type":"event","payload":{"t":"2024-03-20T14:01:00Z","actor":"nora","verb":"observes","object":"practice:pairing","prov":{"file":"demo","line":1}}}'
```
Each line receives an `ack`/`err` JSON line in response.

### MUSN Transport Acceptance Tests
`clojure -M:test` exercises the Prototype 0 brief end-to-end:
- `test/f2/router_test.clj` replays 100 identical `event` frames (same `msg-id`) to ensure idempotent `eid`/`run-id` acks, asserts `export` never touches the scenario path on disk, and validates `run` → `status` transitions return a `:done` state with completion metrics via the mock F3 adapter.
- `test/transport_test.clj` drives the websocket control path so killing one client leaves peers untouched while the shared history logs a `:disconnect`, proving the per-client supervision rule in the MUSN spec.

### Groundhog Day demo (in progress)
- The deterministic transcript captured in `../futon5/resources/demos/groundhog_day_raw.json` (via `futon5.llm.relay`) is the canonical story we replay through MUSN. After the Futon3→Futon1 adapter lands we will ship a `make groundhog` target that turns this JSON into hello/event/workday/check frames, streams them through `/musn/ingest`, and verifies Futon1 receives matching proof/workday entities.
- `scripts/groundhog_day_ingest.clj` converts the JSON transcript into `dev/groundhog_day.ndjson` plus a clock-out snippet. Run it whenever you refresh the Futon5 capture.
- `scripts/run_groundhog_day.sh` posts the resulting NDJSON to `/musn/ingest` so you can replay the loop without opening Emacs.
- `scripts/groundhog_day_push_futon1.clj` posts the same NDJSON into a Futon1 API profile (default `testing`). Set `FUTON1_API_BASE`/`FUTON1_PROFILE` as needed so the demo appears under the right profile.

Non-interactive run:
```bash
cd futon3
./scripts/groundhog_day_ingest.clj
./scripts/run_groundhog_day.sh
FUTON1_API_BASE=http://localhost:8080/api/alpha \
FUTON1_PROFILE=testing \
  ./scripts/groundhog_day_push_futon1.clj
```
This reproduces the hello→event→workday→check→bye flow and mirrors the
proof/workday entities into Futon1 without touching the HUD.

## Drawbridge (nREPL over HTTP)
You can expose a full nREPL endpoint over HTTP using Cemerick Drawbridge. It is disabled by default—add `{:drawbridge {:enabled? true ...}}` when calling `musn/start!`, or use the helper make target.

```
(musn/stop!)
(def system (musn/start! {:drawbridge {:enabled? true
                                      :bind "127.0.0.1"
                                      :port 6767
                                      :allow ["127.0.0.1" "::1"]
                                      :token (System/getenv "ADMIN_TOKEN")}}))
```

If no token is supplied (env var or explicit), the Drawbridge listener is skipped for safety.

### `make repl`
```
make repl   # reads $ADMIN_TOKEN or .admintoken, starts Drawbridge on 127.0.0.1:6767
```

### Emacs / CIDER
1. Start Drawbridge locally or tunnel it (`ssh -N -L 6767:127.0.0.1:6767 user@server`).
2. `M-x cider-connect-clj-http`
   - URL: `http://localhost:6767/repl`
   - Headers: add `X-Admin-Token: <your-token>`.

### CLI sanity check
```
curl -s -H "X-Admin-Token: $$ADMIN_TOKEN" \
     -X POST http://localhost:6767/repl \
     -d '(+ 1 2 3)'
# => 6
```

Keep the listener bound to `127.0.0.1` (default) and tunnel when working remotely, or place it behind HTTPS + a reverse proxy. Never reuse your app/admin token here.

## UI Routes
- `GET /musn/clients` – connected clients, remote addr, current REPL mode, last activity
- `GET /musn/sessions` – recent history grouped by client (including SAFE eval outputs)
- `GET /musn/practices` – reasoning stubs (link suggestions + pattern instances)
- `GET /musn/tatami/status` – 24h summary of tatami sessions (emoji/fruit counts)

## Vitality feeds (git activity)

FUTON0 collects raw git stats via `scripts/git_activity.py` (run it from the `futon0`
repo with `python3 scripts/git_activity.py --days 60 --output ../futon3/resources/vitality/git_activity.json`).
Futon3 then converts that JSON into a HUD-friendly EDN snapshot:

```bash
clojure -M:vitality/git-summary
# writes resources/vitality/git_summary.edn
```

Load the summary in code via `futon3.vitality.git` – it exposes helpers such as
`load-summary`, `daily-grid`, `streak`, and `dominant-sphere` so Tatami HUD panels
can render semantic grids (“which sphere dominated today?”, “how long since the last
commit?”) without touching git directly.

## Sigil adjacency builder (emoji / hanzi)

A fake embedding is useful when Futon3 needs to “snap” a devmap clause onto the closest pattern in the standard library.

1. Annotate every new flexiarg conclusion (or equivalent top-level clause) with one or two `emoji/hanzi` sigil pairs inside brackets, e.g. `[🏡/工 🔃/入]`. Draw from the existing palette in `resources/sigils/index.edn` (or from the FUTON truth-table / Tokipona baselines) so the helper recognises them.
2. Save the flexiarg in `library/` (or a devmap under `holes/`) and make sure the sigils reflect its intent—these strings become the coordinates for the fake embedding.
3. Rebuild the matrices so the new sigils are part of the embedding space:

```bash
clojure -M:sigils
# scans holes/ and library/ for devmaps + flexiargs and writes resources/sigils/emoji-adjacency.csv, hanzi-adjacency.csv, and index.edn
```

The matrices are cheap to recompute, so rerun the script whenever you add or edit flexiargs/devmaps with fresh sigils.

4. When the same `[emoji/hanzi]` pair shows up repeatedly across devmaps, treat it as an alarm bell: inspect the overlapping clauses, decide whether to split/merge/refine the underlying patterns, and add/retire entries accordingly (see `library/library-coherence/pattern-differentiation-alarms`).
   The current collision list lives in `docs/sigil-collisions.md` so you can see
   which clauses are competing for the same pair before refreshing the HUD.

## Flexiformal Proofwork Deliverables

### Transport contract & golden transcripts
- Prototype 0 in `holes/futon3.devmap` now has a frozen check-job transport contract plus a sandbox profile for deterministic replays. The specs in `docs/protocol/transport-contract-v1.md`, `docs/protocol/golden-transcripts.md`, and `docs/sandbox/README.md` explain what MUSN check jobs expect on the wire, how the NDJSON transcripts prove request -> reply behavior, and how to run the sandboxed Clojure runner so those transcripts stay byte-for-byte reproducible.

### Pattern canon & similarity field
- Prototype 1 + Prototype 2 in `holes/futon3.devmap` describe the canonical pattern store, sigil metadata, and fake embedding that turn FUTON3 into the “pattern library of record.” The library already lives under `library/` (e.g., the devmap-coherence IFR set + prototype-alignment checks) and comes with sigil indices in `resources/sigils/{index.edn,patterns-index.tsv}` plus density visualisations.
- `scripts/pattern_index.py`, `scripts/reembed_devmap_tfidf.py`, and the `:sigils` alias wire those EDN/CSV files into TF–IDF + sigil matrices so DEV/CI can rebuild the fake embedding whenever new flexiargs land. This is the evidence for “pattern canon with embeddings,” not just prose.
- The “classical” helper exposed in Emacs (`my-futon3-embed-english`) reads the very same `resources/sigils/patterns-index.tsv` that the TF–IDF job emits: the job harvests hotwords + section tags from devmaps, while the helper consumes them to score free text against the catalog and surface the nearest clause (now echoed in the Tatami HUD). In other words, the hotword scan and TF–IDF build are two views over one dataset, not competing embeddings.
- `library/devmap-coherence/README.md` captures the current canon map (mojo/or/p4ng/transition/t4r) and lists the IFR/prototype families that keep the devmaps honest. The canonical store now lives in Futon1 (XTDB) and the filesystem is treated as a checked-out working copy; run `scripts/pattern_sync.clj` to push library edits into Futon1 before relying on the API, and `scripts/pattern_pull.clj` to pull Futon1 changes back into files. `resources/pattern_store.edn` is a legacy cache only, while the shared library-coherence patterns (e.g., `library/library-coherence/*.flexiarg`) describe the meta checks that keep every source synced.

### Check DSL & devmap coherence runs
- Prototype 3 in `holes/futon3.devmap` + the `ifr-f3-piti.flexiarg` clause spell out the `check!` interface (pattern id, context EDN, evidence refs → proof state + follow-on obligations). `library/devmap-coherence/*.flexiarg` and the shared `library/library-coherence/*.flexiarg` meta patterns are the actual DSL-ready specs, not hypotheticals.
- `holes/AGENTS.md` turns that intent into a concrete plan: obligation extraction from the devmaps, sigil-driven candidate narrowing, `check!` evaluation, and proof exports back into FUTON1/FUTON2. This document is the evidence that the check DSL is wired into the daily “devmap coherence” cadence.

### CYOA devmap refinement (prototype workflow)
- A CYOA runner now drives per-pattern interviews, Evidence Engineer prompts, and deterministic patching for `holes/futon*.devmap` entries (see `scripts/cyoa_room.clj` and logs under `holes/logs/cyoa`).
- Skip rules in `resources/cyoa/skip_rules.edn` prevent re-running evidence generation when a block already has `+ next-evidence:`.
- Pattern Critic runs as a separate turn, emits inline `@question:` prompts for ambiguous clauses, and can apply them directly to the devmap (`--apply-critic`).
- Flexiarg mode highlights `@question:` lines with warning face to keep clarifications visible during edits.

### Trail instrumentation & proof-state journal
- Prototype 4 in `holes/futon3.devmap` plus the IFR patterns (e.g., `library/devmap-coherence/ifr-f5-samadhi.flexiarg`) demand that proof trails record `:pattern/id`, `:obligation/id`, tags, and joy deltas. These instructions are backed by the live Tatami capture under `resources/tatami-events.edn`, which shows current session-trail fixtures used by Tatami/Tatami-monitor, and `resources/tatami-context.edn`, a rolling log of every `FROM-CHATGPT-EDN` HUD payload that the Emacs integration collects.
- The vocabulary that feeds those traces lives in `resources/type_vocab.txt` and `resources/sigils/compressions.edn`, ensuring we tag trail actions with the same ontology as the pattern canon.

### Workday instrumentation & ChatGPT/Tatami bridge
- Prototype 5 in `holes/futon3.devmap` calls for a `workday/submit` endpoint plus bridges into tooling. The Emacs integration that already exercises this flow now lives in this repo as `contrib/aob-chatgpt.el` (mirrors the futon1 version) so FUTON3 can claim and evolve it alongside the transport. Prompt text files such as `contrib/futon0-clock-out.prompt` now sit beside the helper (override `my-futon-prompt-directory` if you keep prompts elsewhere). The helper ingests prompts, spins up Tatami (`tatami.el`), and streams the pattern/type vocab coming from `resources/sigils/patterns-index.tsv` + `resources/type_vocab.txt` into classical embedding heuristics.
- These assets, together with Tatami shell scripts under `../futon1/contrib/` (referenced in the file), prove that the workday bridge is operational: ChatGPT/Tatami sessions emit NDJSON trails, get scored via the fake embedding, and will become first-class API calls once the `workday/submit` surface in transport lands.
- Clocking out now runs automatically when you close a Futon chat buffer: the helper inserts `contrib/futon0-clock-out.prompt`, captures the EDN summary, and (when FOCUS headings are present in your Org agenda) surfaces a small “d = mark done, s = skip” mini-game so you can confirm AI-suggested completions before they propagate into FUTON1/FUTON3. Reopen the review via `M-x my-chatgpt-shell-open-last-focus-review` if you dismiss it; each run is also logged into FUTON1 as a `:clock-out/summary` entity with an efficacy score derived from your mini-game responses.
- Boundary scans now track the “negative space” around every futon layer: run `scripts/devmap_readiness.py` to lint `holes/futon*.devmap` files and write `resources/boundary.edn` with last-touch timestamps, TODO counts, and evidence gaps for F0–F7. This snapshot, together with the git vitality summary and Futon0’s scanner output, now feeds the Stack HUD block inside the Tatami context so readiness drift (e.g., “f4 missing evidence for 13 prototypes”) appears alongside the Tai Chi/curfew reminders.
- Load `scripts/boundary_hud.el` in Emacs and run `M-x futon-boundary-hud` to see a Magit-style table for F0–F7: each row shows the 🟢/🔴/🟣 orb (based on missing evidence ratio), last devmap touch, and outstanding TODOs. The buffer reads `resources/boundary.edn`, so rerun `scripts/devmap_readiness.py` to refresh the data whenever you update a devmap.
- Need a copy/paste briefing for agents? Run `python3 scripts/futon_summary.py futon0` (or any `fX`) to print the devmap’s `@state` block plus the latest boundary entry from `resources/boundary.edn`. That gives downstream tools a concise “state + derivative” snapshot.
- Tatami cues / pāramitā readouts inside the Emacs HUD now come from the same embedding path we use during batch processing. Each FROM-CHATGPT entry (tatami intent + pattern list) is posted to `/musn/cues`, which runs `futon3.cue-embedding/entry-intent-cues` server-side and returns the fruit × orb projection plus the structured `:cue/intent` metadata. When Tatami doesn’t emit any patterns, the service falls back to the descriptive corpus built by `scripts/build_fruit_orb_corpus.clj` (see `resources/sigils/fruit-orb-corpus.edn`), so plain English like “Boundless friendliness toward all beings” still lands on the expected pāramitā. Emacs renders those values in place of the old static prototype defaults, so the HUD mirrors the exact reasoning on every turn. See `docs/cues-and-orbs.md` for the scoring pipeline and `docs/hud-pipeline.md` for the end-to-end HUD flow.

#### Tatami HUD quick reference
- `M-x my-chatgpt-shell-toggle-context` opens the HUD beside the active `chatgpt-shell` buffer. Each refresh calls `my-futon3-fetch-hints`, so the cue percentages update only when Tatami has received a fresh `/musn/hints` response.
- The raw HUD payloads are appended to `resources/tatami-context.edn` every turn (see `my-chatgpt-shell-persist-edn`). Inspecting that log shows the exact `:fruits` / `:paramitas` objects (IDs, scores, summaries) plus the `:cue/intent` metadata (tokens, matched patterns, fallback hits) that produced the on-screen percentages, so you can confirm the porcelain matches the plumbing created by `futon3.cue-embedding`.
- If the numbers look stale, run `M-x my-chatgpt-shell--refresh-context-buffer` (or submit a new message) to force another `/musn/hints` fetch; the HUD is stateless beyond the captured EDN.
- `C-c C-a` updates the “clocked” pattern slug for the current prototype, letting you keep the HUD focused on the clause you are actively editing. `C-c C-e` opens that pattern via Futon4’s `arxana-patterns-open` so you can edit it in place and then pop back to the chat buffer.
- Saving the pattern inside the Futon4 buffer (`C-c C-s`) automatically records a `prototype → pattern` link in Futon1 for the currently clocked session, so future audits know which clause you were editing.
- The “Tatami cues” (fruits) and “Pāramitās” lines inside the HUD come straight from the Tatami hints API. Every Tatami response includes structured fruit/paramitā entries (`:fruit/id`, `:emoji`, `:summary`, `:score`). `contrib/aob-chatgpt.el` sorts them by score and renders a short description (“🍒 doable (20%) — Simple, low-friction…”, “🟣 truth (12%) — Purity, directness…”), so the cues you see are exactly the ones Tatami reported for the current session.
- Run `clojure -M:tatami-cues` to materialise the cue embeddings captured so far; the script reads `resources/tatami-context.edn`, computes fruit/paramitā vectors via `futon3.cue-embedding`, and writes the annotated timeline to `resources/tatami-cues.edn` for downstream analysis.
- Use `M-x my-chatgpt-shell-replay-last-turn` to reapply the most recent FROM-TATAMI/FROM-CHATGPT EDNs locally (no network calls). This lets you iterate on HUD rendering or salients offline by re-running the last turn’s snapshots.
- A `Salients:` line now surfaces whether the HUD is showing fresh hints, a FROM-CHATGPT fallback, or prototype defaults (e.g., `sigils=FROM-CHATGPT`, `cues=fallback (No response from Futon3)`). If `/musn/hints` or `/musn/cues` fail, the fallback reason is rendered inline so you can spot “stale” line‑2 embeddings or missing fruits immediately.
- **Future cleanup:** all of these HUD stats are held in dozens of buffer-local
  vars (`my-chatgpt-shell-last-inbound-edn`, etc.) that we shuttle between the
  chat buffer and the HUD clone. This keeps biting us when updates run in the
  wrong buffer. We plan to replace that apparatus with a single per-chat map
  stored in a central registry (hash keyed by buffer) and swap it atomically,
  so the HUD logic stops juggling copies. Tracking it here so the refactor
  doesn’t get lost.

##### Intent sigil refresh lifecycle

Intent sigils are recomputed twice per chat turn so the HUD stays responsive
even while Tatami is processing:

1. **Outbound (pre-send).** Pressing `RET` triggers
   `my-chatgpt-shell--build-inbound-edn`, which posts the current intent and the
   *previous* turn’s reasoning events to `/musn/hints`. This keeps the prototype
   defaults warm, but it also logs `[DEBUG] Embedded: …` with sigils derived
   from the last turn. That is expected: no new Tatami evidence exists yet.
2. **Inbound (post-reply).** When Tatami returns a
   `FROM-CHATGPT-EDN` payload, `my-chatgpt-shell--apply-chatgpt-edn` saves it to
   `my-chatgpt-shell-last-edn` and calls
   `my-chatgpt-shell--refresh-context-hints`, which re-runs the embedding using
   the *fresh* intent, events, and patterns. The new sigils land under
   `:intent-sigils`, the HUD buffer is re-rendered immediately, and a second
   `[DEBUG] Embedded: …` line appears—this time capturing the up-to-date pairs.

Because of this two-phase loop it is normal to see the previous turn’s sigils in
the debug log right after sending a prompt. They are automatically replaced when
the reply arrives. If the second refresh never happens, check `*Messages*` for
`/musn/hints` failures or verify that the Tatami response contained a
`FROM-CHATGPT-EDN` block for `my-chatgpt-shell--apply-chatgpt-edn` to ingest.

### Workday submit & `check!` handlers
- `type:"workday"` envelopes (WS or `POST /musn/ingest`) accept `{activity, evidence?, sigils?, prototypes?}` payloads, normalize them, and append to `futon3/logs/workday.edn`. Replies include `run-id`, `workday/id`, and `blocked-by [:f1.persistence]` to keep the dependency on Futon1 explicit until the adapter lands. The helper also exposes `futon3.workday/set-log-path!` for tests.
- `type:"check"` envelopes call the new `futon3.checks/check!` DSL. It loads `resources/sigils/patterns-index.tsv`, matches hotwords against the supplied `context` + `evidence`, logs proof states under `futon3/logs/checks.edn`, and returns `{status, missing, derived/tasks, proof}` so downstream futons can route the result. `check!` is also exposed to the SAFE REPL via `musn.api` bindings.
- Workday submissions can embed `{:check {:pattern/id ...}}` (or just `:pattern/id`), and the router will automatically run `check!` with the submitted activity/evidence before replying. This ties Prototype 3 + Prototype 5 together: instrumentation yields immediate applicability verdicts and proof trail lines even before FUTON1 persistence is wired in.
- Set `FUTON1_API_BASE` (e.g. `http://localhost:8080/api/alpha`) and `FUTON1_PROFILE` (e.g. `testing`) before `make dev` if you want Futon3 to mirror every workday/check result into Futon1 automatically. When these env vars are present the transport posts each workday/check to Futon1 so demo runs appear under that profile without extra scripts.

## Learn-or-Act Retrieval Loop

- `/musn/hints` now returns the learn-or-act envelope instead of bare pattern scores. Every response carries `:search/best-score`, `:search/candidates`, `:search/attempts`, `:failure/mode`, `:stub/current`, and `:gap/current` so Tatami events and dual-channel logs can persist the reasoning trace verbatim.
- Failure causes are explicit enums: `:failure/retrieve` (triage or micro-question needed), `:failure/represent` (insufficient EDN framing), and `:failure/gap` (library missing a fit). Success cases leave `:failure/mode` unset.
- True gaps emit two structures: a gap artifact persisted to `resources/futon1-gaps.edn` (uuid, timestamp, short context, why-codes, nearest known IDs, proposed title) and a provisional stub with only context/problem/first-moves/success bullets. Futon3 always tells the user when Gap Mode activates before riffing on the stub.
- Triaged-but-ambiguous searches surface as `:failure/retrieve` with a single-choice micro question (“Is this closer to A/B/C?”) so the caller can tighten the query instead of hallucinating.

- `POST /musn/export` – writes `dev/scenario.edn` snapshot to disk

## Running Tests
```bash
make test
```
Runs Datascript fixture tests, semantics checks, and transport/REPL dispatch tests.

### Emacs Test Tips
`scripts/test-elisp.sh` runs the chatgpt-shell HUD suite. To keep it gentle on memory:

- limit the scope with `ERT_SELECTOR`, e.g. `ERT_SELECTOR='futon3-prototype-*' ./scripts/test-elisp.sh`
- cap virtual memory when running the full suite: `ulimit -v 2097152 && ./scripts/test-elisp.sh`

There’s also a focused embedding test (`futon3-intent-sigils-refreshes-with-pattern-overlap`) that
exercises the HUD intent line without talking to Tatami.

## Next Steps
- Swap the Datascript placeholder with the upcoming external graph/persistence package
- Expand the `musn.api` DSL surfaced to SAFE evals (graph queries, semantic helpers, etc.)
- Layer auth/tooling on top of the `eval` frame (CLI helpers, per-token quotas, observability)
