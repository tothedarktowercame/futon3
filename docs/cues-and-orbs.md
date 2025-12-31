# Cues & Orbs Pipeline

This note explains how Tatami computes the “Tatami cues” (fruits) and
Pāramitā orbs that appear in the HUD and in the `/musn/cues` API. It also
captures where to find fixtures and how to extend the automated tests.

## Runtime flow

1. Every `FROM-CHATGPT-EDN` payload includes the current intent text,
   Tatami pattern matches, and the event log. Emacs forwards that entry to
   `/musn/cues` (see `my-chatgpt-shell--enrich-with-cues`).
2. The router calls `futon3.cue-embedding/entry-intent-cues` with that
   entry plus the active pattern list.
3. `entry-intent-cues` tokenizes the intent text and indexes each pattern
   (`:title`, `:summary`, `:id`). Tokens that overlap between intent and a
   pattern contribute to the `weight` of that pattern. The weight equals the
   size of the token overlap plus the `similarity-from-distance` derived
   from Tatami’s pattern score. High-weight patterns (defaults: top 3) are
   treated as the “anchors” for cue selection.
4. For each high-weight pattern we pull its Tokipona/TruthTable sigils. The
   combined sigils feed `futon3.pattern-hints/fruits-for-sigils` and
   `/paramitas-for-sigils`, which in turn look up the weighted fruits/orbs
   using `resources/sigils/fruit-orb-corpus.edn` as the canonical reference.
5. The scores Tatami returns are already normalized floats in the `[0, 1]`
   range; the HUD simply formats them as percentages. When Tatami fails to
   emit any patterns we replay the descriptive fruit/orb corpus so that
   plain-English intents still activate the right pāramitā.

## Off-line scripts and fixtures

- `scripts/cue_embedding.clj` reads `resources/tatami-context.edn` and
  materialises the full cue/orb embeddings into
  `resources/tatami-cues.edn`. Run `clojure -M:tatami-cues` whenever you
  update the corpus or change the scoring rules and want an auditable log.
- HUD fixtures captured from real sessions live in
  `resources/hud-fixtures.edn`. In Emacs, call
  `M-x my-chatgpt-shell-append-hud-fixture` after a representative turn to
  append the current inbound ChatGPT/Tatami payloads together with any cue
  errors. The ERT test `futon3-hud-fixtures-replay-cues` replays those
  fixtures to make sure the HUD renders the exact same fruit/orb lines and
  guidance text.

## Debugging checklist

* Confirm Tatami returned patterns with sigils. Without them the cue
  embedding has no seed vectors. Use `/musn/hints` (or the HUD pattern
  verdict) to see the raw pattern list.
* If the cues look like they belong to a different chat buffer, check
  `my-chatgpt-shell--hud-state-registry` and rerun
  `my-chatgpt-shell--maybe-render-context`. Each refresh now records a per
  buffer snapshot, and `my-chatgpt-shell--warn-on-hud-drift` emits a debug log
  when the registry copy of `:paramitas` diverges from the live chat buffer.
* When the HUD shows `cues=fallback` or `cues=missing`, check
  `my-chatgpt-shell--last-hints-error` and `my-chatgpt-shell--last-cues-error`
  (they are also recorded in the fixture log). Those errors come straight
  from the `/musn/hints` and `/musn/cues` adapters.
* To inspect the exact scoring inputs point `script/cue_embedding.clj` at a
  saved `resources/tatami-context.edn` log and look at the resulting
  `resources/tatami-cues.edn` entry—the file contains the intent tokens,
  which patterns fired, the computed weights, and the normalized fruit and
  pāramitā vectors.

With these notes on hand you can trace any pair of HUD cues back to the
intent text, pattern matches, and the sigil → fruit/orb projection used by
Tatami.
