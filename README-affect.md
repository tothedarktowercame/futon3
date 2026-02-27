# Affect Signal Processing for Claude Code / Codex

This document describes how to wire up Claude Code or Codex sessions to flow through Labs with affect signal detection.

## Overview

The affect system:
1. Monitors conversation turns for affect-laden language (joy, anxiety, frustration, etc.)
2. Tracks "pending affects" with a 10-minute lookahead window
3. Detects novel terms that appear after affect signals
4. Emits `affect/transition` events when pending affects expire with accumulated terms

## Architecture

```
Claude Code / Codex
        |
        v
   UserPromptSubmit hook
        |
        v
   claude-activity-hook.sh
        |
        +---> /musn/session/create (ensures Labs session exists)
        |
        +---> /musn/scribe/turn (logs turn, triggers affect processing)
        |
        +---> /musn/activity/log (vitality tracking)
        |
        v
   record-turn! (in futon3)
        |
        v
   futon3a.affect/process! (async)
        |
        +---> nlp-interface.intent/analyze (keyword-based affect detection)
        |
        +---> futon1 /api/alpha/nlp/entities (Stanford CoreNLP entity extraction)
                     |
                     +---> NER: person, org, place, date
                     +---> NP chunks: noun phrases
                     +---> Returns: entity labels for novelty tracking
```

### NLP Entity Extraction

When `use-nlp-entities?` is true (default), the affect processor calls futon1's
NLP API to extract proper entities from text using Stanford CoreNLP:

- **Named Entity Recognition (NER)**: Extracts person names, organizations, places, dates
- **Noun Phrase Chunks**: Extracts meaningful noun phrases from parse trees
- **Entity deduplication**: Returns unique lowercase entity labels

If futon1 is unavailable or the call fails, falls back to regex-based extraction
(words 6+ characters).

## Hook Configuration

### For Claude Code

Add to `~/.claude/settings.json` (or `settings.local.json`):

```json
{
  "hooks": {
    "UserPromptSubmit": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "/home/joe/code/futon3/scripts/claude-activity-hook.sh"
          }
        ]
      }
    ]
  }
}
```

### For Codex

Similar configuration in Codex's settings (location TBD based on Codex config structure).

## Environment Variables

The hook script uses:
- `MUSN_HOST` - defaults to `127.0.0.1`
- `MUSN_PORT` - defaults to `6065`

For remote MUSN (e.g., laptop connecting to Linode):
```bash
export MUSN_HOST=lucy.example.com
export MUSN_PORT=6065
```

## The Hook Script

Located at: `scripts/claude-activity-hook.sh`

The hook:
1. Reads JSON payload from stdin (includes `session_id`, `prompt`, `cwd`)
2. Creates a Labs session if it doesn't exist
3. Logs the turn via `/musn/scribe/turn` with the prompt content
4. Also logs to `/musn/activity/log` for vitality tracking

## Affect Intents Detected

From `nlp-interface.intent`:
- `:activation` - anger, urgency, determination ("fired up", "under pressure")
- `:attraction` - curiosity, fascination ("intrigued", "captivated")
- `:joy` - happiness, contentment ("excited", "delighted", "happy")
- `:fatigue` - exhaustion, being overwhelmed ("tired", "burned out")
- `:anxiety` - fear, worry, stress ("anxious", "worried", "on edge")
- `:withdrawal` - avoidance, being stuck ("holding back", "shut down")
- `:frustration` - annoyance, impatience ("frustrated", "exasperated")
- `:sadness` - depression, disappointment ("sad", "down", "heavy-hearted")
- `:numbness` - detachment, apathy ("numb", "checked out", "dead inside")
- `:orientation` - clarity vs confusion ("clear", "lost", "uncertain")
- `:social` - connection vs isolation ("connected", "lonely", "rejected")
- `:regulation` - balance vs agitation ("calm", "restless", "grounded")

## Testing

### 1. Verify hook is running

Send a message through Claude Code/Codex and check:
```bash
tail -5 /home/joe/code/futon3/lab/activity/log.ndjson
```

### 2. Check Labs session

```bash
curl -s -X POST "http://localhost:6065/musn/session/state" \
  -H "Content-Type: application/json" \
  -d '{"session/id": "YOUR-SESSION-ID"}' | jq '.events | length'
```

### 3. Check server logs for affect processing

Look for in the server output:
```
[affect] Processing turn: :user
[affect] Done processing turn
```

### 4. Manual test

```bash
# Create session
curl -s -X POST "http://localhost:6065/musn/session/create" \
  -H "Content-Type: application/json" \
  -d '{"session/id": "test-affect"}'

# Send affect-laden turn
curl -s -X POST "http://localhost:6065/musn/scribe/turn" \
  -H "Content-Type: application/json" \
  -d '{"session/id": "test-affect", "role": "user", "content": "I am feeling excited and happy about this feature"}'
```

### 5. Debug hook payload

Check `/tmp/claude-hook-debug.log` for hook payload and curl responses.

## Configuration

The affect processor settings are in `futon3a/src/futon3a/affect.clj`:
- `lookahead-minutes`: 10 (window for novel terms after affect)
- `novelty-minutes`: 43200 (30 days - term is "novel" if not seen recently)
- `max-pending`: 100 (max pending affect events per actor)
- `musn-url`: "http://localhost:6065" (MUSN endpoint for transition events)
- `futon1-url`: "http://localhost:8080" (futon1 API for NLP entity extraction)
- `use-nlp-entities?`: true (use futon1 NLP vs regex fallback)

To configure at runtime via REPL:
```clojure
(require '[futon3a.affect :as aff])
(aff/configure! {:futon1-url "http://lucy:8080"
                 :use-nlp-entities? true})
```

## Stack HUD Integration

TODO: Add affect summaries per day to Stack HUD with cursor-over details.

## Files

- `scripts/claude-activity-hook.sh` - Hook script for Claude Code/Codex
- `src/futon3/musn/service.clj` - `record-turn!` with affect processing
- `../futon3a/src/futon3a/affect.clj` - Affect processor
- `../futon1/apps/nlp-interface/src/nlp_interface/intent.clj` - Intent/affect dictionary
- `../futon1/apps/api/src/api/handlers/nlp.clj` - NLP entity extraction endpoint
- `../futon1/apps/open-world-ingest/src/open_world_ingest/nlp.clj` - Stanford CoreNLP pipeline

## Futon1 NLP Endpoints

### POST /api/alpha/nlp/entities

Lightweight endpoint for entity extraction (no storage).

**Request:**
```json
{"text": "I met John Smith at Google yesterday"}
```

**Response:**
```json
{
  "text": "I met John Smith at Google yesterday",
  "labels": ["john smith", "google", "yesterday"],
  "entities": [
    {"label": "John Smith", "kind": "person", "lower-label": "john smith"},
    {"label": "Google", "kind": "org", "lower-label": "google"},
    {"label": "yesterday", "kind": "date", "lower-label": "yesterday"}
  ],
  "count": 3
}
```

### POST /api/alpha/nlp/analyze

Full NLP analysis with entities and relations.

**Request:**
```json
{"text": "Joe works at Anthropic in San Francisco"}
```

**Response:**
```json
{
  "text": "Joe works at Anthropic in San Francisco",
  "entities": [...],
  "entity-count": 3,
  "relation-count": 2
}
```
