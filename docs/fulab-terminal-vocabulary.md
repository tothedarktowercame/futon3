# Fulab Terminal Vocabulary (Draft)

This document defines the **terminal vocabulary** for FuLab agents: the minimal
set of actions and signals that should appear in a session log. It is derived
from the AIF selection loop and the MUSN tooling surface.

The vocabulary is **terminal** in the sense that every run can be decomposed
into these steps, even if the agent uses richer internal reasoning.

## Scope

- Applies to FuLab agents (fucodex, fuclaude, fubar) operating with AIF.
- Uses the existing MUSN endpoints and helper scripts.
- Focuses on **what to log**, not on implementation internals.

## Core Cycle (one turn)

```
Session start
  → turn plan
  → pattern selection (PSR)
  → pattern application (actions + tools)
  → observe outcomes
  → belief update (AIF summary)
  → pattern use (PUR)
  → turn end
```

### AIF selection sketch

Pattern selection uses softmax over expected free energy:

```
logit = -G / τ
prob(pattern) = softmax(logit)
```

Exploration is triggered when τ is low (or explicit explore flag).

## Terminals

Each terminal has a **signal**, a **purpose**, and a **mechanism**.

### Session Terminals

| Terminal | Purpose | Mechanism |
| --- | --- | --- |
| `session/start` | Begin a FuLab session (clock‑in). | `fucodex --live` or `fulab/clock-in` pattern action. |
| `session/intent` | Record intent + scope handshake. | `:session/intent-handshake` event (see `docs/fulab-plan.md`). |
| `session/resume` | Continue an existing session. | `fucodex --live --session-id ... resume ...` |

### Turn Terminals

| Terminal | Purpose | Mechanism |
| --- | --- | --- |
| `turn/plan` | Plan before tool use. | `/home/joe/code/futon3/scripts/musn-plan "Plan: ..."` |
| `turn/select` (PSR) | Log candidate patterns + choice. | `/home/joe/code/futon3/scripts/pattern-select ...` |
| `turn/action` | Log pattern actions (read/update/implement). | `/home/joe/code/futon3/scripts/pattern-action ...` or `/musn/turn/action` |
| `turn/use` (PUR) | Log pattern use and outcome. | `/home/joe/code/futon3/scripts/pattern-use ...` |
| `turn/end` | Close the turn and emit summary. | `/musn/turn/end` (via MUSN service) |

### Tool / Evidence Terminals

| Terminal | Purpose | Mechanism |
| --- | --- | --- |
| `tool/use` | Capture command/tool calls. | Auto‑captured in lab stream (tool uses list). |
| `evidence/add` | Attach evidence anchors to patterns. | `/musn/evidence/add` (via service; auto hooks in fucodex). |
| `chat/message` | User contact / question. | `/home/joe/code/futon3/scripts/musn-chat ...` |

### AIF Terminals

| Terminal | Purpose | Mechanism |
| --- | --- | --- |
| `aif/summary` | Belief update + precision (τ). | Auto‑emitted in AIF adapter. |
| `aif/tap` | Raw AIF instrumentation for selection/update. | Auto‑emitted in AIF adapter. |

## Required Fields (Minimum)

These are the minimum fields expected by MUSN services and reports.

### PSR (Pattern Selection Record)

- `session/id`
- `turn`
- `candidates` (vector of pattern ids)
- `chosen` (pattern id)
- `reason` (map)
  - `:mode` (`:read` / `:use`)
  - `:source` (`:explicit` / `:auto`)
  - `:note` (optional)
  - `:aif` (optional: G scores, τ)

### PUR (Pattern Use Record)

- `session/id`
- `turn`
- `pattern/id`
- `note` (optional; anchor summary)
- `anchors` (optional; files, commands, tests)

### Pattern Action

- `pattern-id`
- `action` (`read` / `update` / `implement` / `question` / `off-trail`)
- `note` (why this action fits)
- `files` (optional; for update/implement)

## Operational Constraints

- **Select before act**: `turn/select` must precede `turn/action`.
- **Evidence hygiene**: attach files / tool outputs whenever possible.
- **Exploration**: when τ is low, prefer `pattern-select --auto` or log an
  explicit explore choice in the PSR reason.
- **Auto hooks**: fucodex logs reads and updates automatically when HUD context
  is active; do not double‑log unless needed.

## Quick Reference (CLI)

```
/home/joe/code/futon3/scripts/musn-plan "Plan: ..."
/home/joe/code/futon3/scripts/pattern-select library/<pattern> "why"
/home/joe/code/futon3/scripts/pattern-use library/<pattern> "where applied"
/home/joe/code/futon3/scripts/pattern-action read|update|implement library/<pattern> "note"
/home/joe/code/futon3/scripts/musn-chat "question for user"
```

## Notes

- FuLab uses the futon2 AIF adapter; the terminal vocabulary keeps the surface
  stable even as the internal adapter changes.
- This doc complements `docs/fucodex-aif.md` and `docs/aif-pattern-engine.md`.
