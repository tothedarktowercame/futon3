# Mission: PAR as Session Punctuation

## Problem

Long-running sessions suffer from context drift. The session ID persists but the effective working memory shifts - both agent and human forget what they set out to do days ago.

Current session streams are undifferentiated: `event → event → event → ...` - an indefinite "and, and, and" with no structure.

When context overflows and sessions resume, there's no reliable way to recover intent.

## Solution

Make PARs (Post-Action Reviews) first-class events within MUSN sessions:

1. **Session-native** - PARs are events in the stream, not external artifacts
2. **Phase markers** - they punctuate the stream into meaningful subsequences
3. **Forum-relayed** - automatically posted so other agents can sync
4. **On-demand** - available any time, not just session end

## Design

### PAR as Event

```clojure
{:event/type :session/par
 :at #inst "2026-01-31T..."
 :par/id "par-abc123"
 :par/sequence 1  ;; nth PAR in this session
 :par/span {:from "event-id-1" :to "event-id-42"}  ;; events covered
 :par/questions
 {:intention "what we expected to learn/make"
  :happening "what and how we're learning"
  :perspectives "different views on what's happening"
  :learned "what we learned or changed"
  :forward "what else should we change"}
 :par/tags [:checkpoint :phase-complete :blocked ...]}
```

### Stream Structure

Before:
```
event → event → event → event → event → event → ...
```

After:
```
event → event → event → PAR₁ → event → event → PAR₂ → ...
                         ↓                       ↓
                    [Forum post]           [Forum post]
```

### Forum Relay

When a PAR is created:
1. Post to session's Forum thread (or create one if none exists)
2. Include session ID, PAR sequence, and the 5-question summary
3. Tag with session metadata for filtering

### Recovery Flow

When context overflows and session resumes:
1. Read last N PARs from the session
2. Read Forum thread for cross-agent context
3. Agent has structured catch-up material, not raw event stream

## Integration Points

- **MUSN service** - new event type `:session/par`
- **Forum service** - relay logic from PAR events
- **fuclient-logs.el** - render PARs as section headers in stream
- **arxana-lab.el** - PARs as navigation anchors in session viewer
- **Agent prompts** - encourage PAR emission at natural breakpoints

## The 5 Questions

From Joe's traditional PAR template:

1. **Review the intention**: what do we expect to learn or make together?
2. **Establish what is happening**: what and how are we learning?
3. **What are some different perspectives** on what's happening?
4. **What did we learn or change?**
5. **What else should we change going forward?**

## Example PAR (from this session)

**Intention**: Build F6 infrastructure, establish real-time agent coordination

**Happening**: Shipped incremental fixes, discovered existing machinery (fuclient-logs, pattern_competence), found PSR/PUR/PAR scaffolding is dormant

**Perspectives**:
- Builder: plumbing works, time to turn on water
- User: too many pieces, unclear what's usable
- System: loop not closed (PSR→PUR→PAR→patterns)

**Learned**: WebSocket transport ready, encyclopedia pipeline works, pattern machinery elaborate but dormant

**Forward**: Activate the loop, run PARs habitually, less scaffolding more usage

## Status

- [x] Design sketched
- [ ] MUSN event schema implemented
- [ ] Forum relay logic
- [ ] fuclient-logs PAR rendering
- [ ] Agent prompt integration
- [ ] First real PAR emitted and relayed

## Origin

Session: 2026-01-31, London Linode
Participants: joe, claude-opus
Context: Discovered PSR/PUR machinery exists but is unused; paper-prototyped PAR interactively
