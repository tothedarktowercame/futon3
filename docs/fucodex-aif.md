# Fucodex AIF instrumentation

This note explains how AIF scoring is attached to fucodex pattern runs and how to tune it.

## How it connects

- fucodex live mode (`fucodex --live ...`) streams Codex JSON into `dev/lab-stream-codex.clj`.
- On each `turn.completed`, the stream runner emits:
  - `:pattern/selection-claimed` (PSR) and `:pattern/use-claimed` (PUR) events.
  - `:aif/summary` events for the PSR (selection) and PUR (update).
  - `:aif/tap` events for each AIF adapter tap> payload (select/update).
- When an AIF config is supplied, an `:aif/config` event is emitted once per session.

The AIF adapter sees the same decision ids and anchors as PSR/PUR, so the AIF summary can be joined to pattern claims by `:decision/id` and `:session/id`.

## Tuning parameters

The fulab AIF adapter reads an optional EDN config. Provide it with:

```
./fucodex --live --aif-config docs/aif/fulab-config.edn exec "..."
```

To see how a config will score a typical turn, use:

```
./fucodex --aif-explain docs/aif/fulab-config.edn
./fucodex --aif-explain
```

Config keys (defaults shown):

```
{:g/weights {:base 0.1
             :anchors 0.05
             :forecast 0.02}
 :tau/scale 1.0
 :tau/min 0.1
 :tau/max 2.0}
```

Interpretation:
- `:g/weights` controls the contribution of candidate text length, anchors, and forecast size.
- `:tau/scale` changes the base precision.
- `:tau/min` and `:tau/max` clamp the resulting tau.

## Session artifacts

A live run produces:
- `lab/sessions/<session-id>.edn` with PSR/PUR + AIF summary/tap events.
- `lab/raw-stream/<thread-id>.jsonl` with the raw Codex JSON stream.
- `lab/aif/<session-id>.edn` with AIF summaries (from the equip pass).

## Resuming a run

To extend a session with additional turns (and additional clock-ins), resume via:

```
./fucodex --live --session-id <session-id> resume --last "Continue work"
```

This appends to the existing session file while keeping the same run id for reports.
