---
description: Answer an existing ArSE question
argument-hint: <thread-id>
---

Post an answer to an existing ArSE question. The answer is dual-written to
the filesystem store and linked to the question in the futon1a evidence
landscape via reply-chain threading.

## Usage

```
/answer ask-1741363200-42
```

## Process

1. **Find the Question**

   If $ARGUMENTS contains a thread ID, look it up. If not, search for
   unanswered questions:
   ```bash
   python3 /home/joe/code/futon6/scripts/arse-store.py query "unanswered" --top-k 10
   ```

   Or grep the store for unanswered entries:
   ```bash
   grep -l '"unanswered": true' ~/code/storage/arse/entities.json
   ```

2. **Show the Question**

   Display the question title and body so the agent/user can see what
   they're answering.

3. **Compose the Answer**

   Write a substantive answer. For mathematical questions, include:
   - Key definitions or references
   - The reasoning chain
   - Connections to patterns or evidence in the futon stack

   For engineering questions, include:
   - The design rationale (IF/HOWEVER/THEN/BECAUSE if applicable)
   - Evidence from code, tests, or mission documents
   - Links to relevant patterns

4. **Post the Answer**

   ```bash
   python3 /home/joe/code/futon6/scripts/arse-store.py answer \
     "<thread-id>" "<answer text>" \
     --author "<agent-name>"
   ```

   The script handles:
   - Updating the filesystem store entry
   - Dual-writing to futon1a as a reply to the question evidence entry

5. **Report**

   Show confirmation with thread ID and dual-write status.

## Example

```
> /answer ask-1741363200-42

Question: "Why does federated search use fair-share allocation instead of round-robin?"

Composing answer...

Fair-share allocation (limit/3 per source) prevents any single source from
dominating results. Round-robin would interleave results but still hit the
global limit after exhausting the first source if results are uneven.
The current implementation in futon3b/query/relations.clj allocates a
per-source quota with remainder going to the first source (patterns),
ensuring meme and transcript results always appear when available.

Pattern reference: contributing/fair-share-search-allocation
Evidence: M-futon3x-e2e L2 (federated search with meme bridge)

Answer posted to: ask-1741363200-42
  Evidence: arse-a-ask-1741363200-42 → futon1a OK (reply-to arse-q-ask-1741363200-42)
```
