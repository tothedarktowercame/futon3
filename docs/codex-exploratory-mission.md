# Codex Exploratory Mission (v0)

This is a reusable prompt for manual exploratory runs (Item 1). It is intended
for fucodex live runs before automated selection is implemented.

## Prompt

```
You are running in exploratory pattern mode. Do not wait for a directive.

OBSERVE
- Read the pattern catalog at resources/sigils/patterns-index.tsv.
- Scan the local codebase for a small, concrete improvement.

SELECT
- Choose one pattern that best fits the current context.
- List 2–4 candidates and state which one you chose.

APPLY
- Make one bounded change guided by the chosen pattern.
- Keep the change small and local.

LOG (PSR/PUR)
- Produce a PSR and PUR in the session trace format.
- Include anchors to concrete session events.

CONTINUE or EXIT
- If you can identify a second bounded change that fits a different pattern,
  repeat OBSERVE → SELECT → APPLY → LOG once more.
- Otherwise, stop and summarize.

Constraints:
- Do not run tests unless explicitly asked.
- Stay within the current repo.
- Prefer edits over new files.
```

## Suggested invocation

```
./fucodex explore --explore-hotwords "pattern,trace"
```

(Adjust hotwords or namespaces as needed; explore mode will pick candidates.)

## Current heuristic (manual)

Until AIF-driven selection is implemented, candidate patterns should be chosen
by a simple heuristic:
- Prefer patterns whose hotwords match the task or files touched.
- Prefer patterns in the relevant namespace (ants/* for AIF behavior, code-coherence/* for hygiene, etc.).
- Avoid repeating a pattern already used in the same session unless necessary.

Plan: replace this manual heuristic with AIF-tuned selection once Item 3 lands.
