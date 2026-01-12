# MUSN viewer: summaries + AIF signals + condensed trail (follow-up)

## Problem
The current MUSN viewer tails the raw stream only. It’s verbose, hard to follow, and misses the compact summary/AIF cues we had in the older lab_stream_codex runner. Recent runs (e.g., E02-C) complete but the viewer does not show a final summary/lab note, nor lightweight AIF lines.

## Goals
- Emit a concise turn summary at `turn.completed` (actions, applied patterns/uses, AIF suggestion, delta). Write a stub/lab note similar to the old `.org` stubs.
- Surface AIF signals in the viewer (select/update/PSR/PUR) as terse lines (`[aif] select … G=… tau=…`).
- Add a condensed view mode: keep raw stream in one buffer, and render a cleaned trail in the viewer (turn start, candidate list, plan/off-trail warnings, selection, uses, summary).
- Reduce log spam: fold/hide low-value command output unless requested.

## Tasks
1. MUSN stream: add `turn.completed` summary emission (JSON + stub). Include applied patterns/uses, AIF suggestion, and a short “delta” section.
2. Viewer: print AIF select/update/use/psr/pur lines in a compact format; optionally toggle verbosity.
3. Viewer: add condensed trail rendering (e.g., minimal formatted buffer alongside raw tail).
4. Validate with a fresh run (E02-C prompt) and ensure HUD still syncs.

Notes
- Drawbridge hotloading works (127.0.0.1:6767 with ADMIN_TOKEN), so we can iterate live.
- HUD already renders candidates/AIF; avoid overwriting MUSN payload during render.
