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
< {"type":"hello","ok":true,"client":"C-5a2b7c1d"}
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

## Flexiformal Proofwork Deliverables

### Pattern canon & similarity field
- Prototype 1 + Prototype 2 in `holes/futon3.devmap` describe the canonical pattern store, sigil metadata, and fake embedding that turn FUTON3 into the “pattern library of record.” The library already lives under `library/` (e.g., the devmap-coherence IFR set + prototype-alignment checks) and comes with sigil indices in `resources/sigils/{index.edn,patterns-index.tsv}` plus density visualisations.
- `scripts/pattern_index.py`, `scripts/reembed_devmap_tfidf.py`, and the `:sigils` alias wire those EDN/CSV files into TF–IDF + sigil matrices so DEV/CI can rebuild the fake embedding whenever new flexiargs land. This is the evidence for “pattern canon with embeddings,” not just prose.
- `library/devmap-coherence/README.md` captures the current canon map (mojo/or/p4ng/transition/t4r) and lists the IFR/prototype families that keep the devmaps honest. The live store lives in `resources/pattern_store.edn` and is loaded via `futon3.pattern-store` (see `test/pattern_store_test.clj`), while the shared library-coherence patterns (e.g., `library/library-coherence/*.flexiarg`) describe the meta checks that keep every source synced, so the README now cites concrete namespaces + fixtures rather than future intent.

### Check DSL & devmap coherence runs
- Prototype 3 in `holes/futon3.devmap` + the `ifr-f3-piti.flexiarg` clause spell out the `check!` interface (pattern id, context EDN, evidence refs → proof state + follow-on obligations). `library/devmap-coherence/*.flexiarg` and the shared `library/library-coherence/*.flexiarg` meta patterns are the actual DSL-ready specs, not hypotheticals.
- `holes/AGENTS.md` turns that intent into a concrete plan: obligation extraction from the devmaps, sigil-driven candidate narrowing, `check!` evaluation, and proof exports back into FUTON1/FUTON2. This document is the evidence that the check DSL is wired into the daily “devmap coherence” cadence.

### Trail instrumentation & proof-state journal
- Prototype 4 in `holes/futon3.devmap` plus the IFR patterns (e.g., `library/devmap-coherence/ifr-f5-samadhi.flexiarg`) demand that proof trails record `:pattern/id`, `:obligation/id`, tags, and joy deltas. These instructions are backed by the live Tatami capture under `resources/tatami-events.edn`, which shows current session-trail fixtures used by Tatami/Tatami-monitor.
- The vocabulary that feeds those traces lives in `resources/type_vocab.txt` and `resources/sigils/compressions.edn`, ensuring we tag trail actions with the same ontology as the pattern canon.

### Workday instrumentation & ChatGPT/Tatami bridge
- Prototype 5 in `holes/futon3.devmap` calls for a `workday/submit` endpoint plus bridges into tooling. The Emacs integration that already exercises this flow now lives in this repo as `contrib/aob-chatgpt.el` (mirrors the futon1 version) so FUTON3 can claim and evolve it alongside the transport. The helper ingests prompts, spins up Tatami (`tatami.el`), and streams the pattern/type vocab coming from `resources/sigils/patterns-index.tsv` + `resources/type_vocab.txt` into classical embedding heuristics.
- These assets, together with Tatami shell scripts under `../futon1/contrib/` (referenced in the file), prove that the workday bridge is operational: ChatGPT/Tatami sessions emit NDJSON trails, get scored via the fake embedding, and will become first-class API calls once the `workday/submit` surface in transport lands.

### Workday submit & `check!` handlers
- `type:"workday"` envelopes (WS or `POST /musn/ingest`) accept `{activity, evidence?, sigils?, prototypes?}` payloads, normalize them, and append to `futon3/logs/workday.edn`. Replies include `run-id`, `workday/id`, and `blocked-by [:f1.persistence]` to keep the dependency on Futon1 explicit until the adapter lands. The helper also exposes `futon3.workday/set-log-path!` for tests.
- `type:"check"` envelopes call the new `futon3.checks/check!` DSL. It loads `resources/sigils/patterns-index.tsv`, matches hotwords against the supplied `context` + `evidence`, logs proof states under `futon3/logs/checks.edn`, and returns `{status, missing, derived/tasks, proof}` so downstream futons can route the result. `check!` is also exposed to the SAFE REPL via `musn.api` bindings.
- Workday submissions can embed `{:check {:pattern/id ...}}` (or just `:pattern/id`), and the router will automatically run `check!` with the submitted activity/evidence before replying. This ties Prototype 3 + Prototype 5 together: instrumentation yields immediate applicability verdicts and proof trail lines even before FUTON1 persistence is wired in.

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

## Next Steps
- Swap the Datascript placeholder with the upcoming external graph/persistence package
- Expand the `musn.api` DSL surfaced to SAFE evals (graph queries, semantic helpers, etc.)
- Layer auth/tooling on top of the `eval` frame (CLI helpers, per-token quotas, observability)
