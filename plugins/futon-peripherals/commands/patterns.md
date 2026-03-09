---
description: Search the pattern catalog by keyword
argument-hint: <search terms>
---

Search the futon pattern catalog and show matching patterns.

## What This Does

The `/patterns` command searches the 850+ pattern catalog by keyword,
returning the best matches with their sigils and rationales. This is the
proper way to find patterns — don't grep the TSV directly.

## Usage

```
/patterns hunger precision     # Search for hunger/precision patterns
/patterns coordination cost    # Search for coordination patterns
/patterns stuck testing        # Describe your situation
```

## Process

1. **Parse Query**

   Extract the search terms from $ARGUMENTS. If empty, ask the user what they're looking for.

2. **Search via HTTP**

   Call the pattern search API:
   ```bash
   curl -s "http://localhost:7070/api/alpha/patterns/search?q=$(echo "$ARGUMENTS" | sed 's/ /+/g')&limit=5"
   ```

3. **Present Results**

   Show matching patterns in this format:

   ```
   ## Pattern Search: <query>

   1. [六] ants/hunger-precision-coupling
      Hunger ↔ Precision Coupling Pattern...
      Hotwords: hunger, precision, tau, affect

   2. [由] aif/evidence-precision-registry
      Making precision explicit turns trust into a tunable control surface...
      Hotwords: evidence, precision, registry
   ```

4. **Suggest PSR**

   If the user wants to carry a pattern, suggest using `/psr <pattern-id>` to
   formally select it and put it in the backpack.

## Example

```
> /patterns pause uncertainty

## Pattern Search: pause uncertainty

1. [不] agent/pause-is-not-failure
   Pausing surfaces uncertainty to those who can resolve it.
   Hotwords: pause, failure, uncertainty, surfaces

2. [认] agent/commitment-varies-with-confidence
   Variable commitment adapts behavior to epistemic state.
   Hotwords: commitment, varies, confidence

Use /psr <pattern-id> to select a pattern for your current work.
```

Begin by searching the pattern catalog via the HTTP endpoint.
