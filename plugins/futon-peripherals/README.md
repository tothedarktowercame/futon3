# Futon Peripherals Plugin

Peripheral mode switching for structured agent workflows.

## Concept

Peripherals are **constrained capability envelopes** - like game modes in a MUD/RPG:
- Inventory mode, shop mode, battle mode, explore mode
- Each mode has specific capabilities and constraints
- Constraints are structural, not behavioral (no compliance theater)
- Memory travels with session-id across hops

## Peripherals

### Explore (Read-Only)

```
/explore <target>
```

Research and understand codebases without making changes.
- CAN: Read, glob, grep, fetch docs
- CANNOT: Edit, write, run modifying commands
- OUTPUT: Understanding, file locations, recommendations

### Reflect (PAR Generation)

```
/par [summary]
```

Generate Post-Action Review at session close.
- CAN: Read session log, analyze patterns
- CANNOT: Edit code, run commands
- OUTPUT: Structured PAR for learning capture

## PAR (Post-Action Review)

The `/par` command generates structured reflection:

```json
{
  "event/type": "session/par",
  "patterns_used": [...],
  "what_went_well": [...],
  "what_could_improve": [...],
  "prediction_errors": [...],
  "suggestions": [...],
  "lineage_trace": {...}
}
```

PARs feed into `stack_learning` metrics in the vitality system.

## Peripheral Hop Flow

```
[Explore] → can read, search
    ↓ hop with memory (session-id)
[Edit] → can modify files
    ↓ hop with memory
[Test] → can verify
    ↓ hop with memory
[Deploy] → can push
    ↓ hop with memory
[Reflect] → generates PAR
```

## Installation

This plugin is part of the futon3 repository. To use in other projects:

1. Copy the `plugins/futon-peripherals` directory
2. Or symlink: `ln -s /path/to/futon3/plugins/futon-peripherals .claude/plugins/`

## Integration with MUSN

PARs can be logged to the MUSN activity stream:

```bash
curl -X POST http://localhost:6065/musn/activity/log \
  -H "Content-Type: application/json" \
  -d @par.json
```

The `/musn/vitality` endpoint aggregates PAR data in `stack_learning`.

## References

- Peripheral spec: `docs/peripheral-spec.md`
- PAR vocabulary: `futon5a/docs/joe-terminal-vocabulary.md`
- Meeting notes: `fulab/meetings/2026-01-27-irc-coordination-review.md`
- fucodex bridge: `scripts/fucodex-chat-bridge.ts` (Codex peripheral example)
