# Mission: Forum Organization (Tags-First)

**Status:** :greenfield  
**Parent:** M-agency-forum (Forum layer), f3/P4 (Mission Queue & Supervisor)  
**Date:** 2026-01-30

## Owner

Claude

## Time Box

4-8 hours for MVP (tags on create/reply, tag filtering, front page endpoint, and documentation).

## Exit Conditions

- Pass criteria are satisfied and documented (including tag conventions).
- If tag-only organization proves insufficient, stop after documenting the failure modes and propose a follow-up mission for schema changes.

## Intent

Make the Forum usable at scale by introducing a tags‑first organization scheme
that supports “one thread per object” (PlanetMath style) while keeping the
front page a unified stream of recent posts.

## Decision (MVP)

Use **tag‑only organization** with namespaced conventions:

- `board/<name>` — high‑level grouping (Agency, Forum, Patterns, etc.)
- `obj/<id>` — one thread per object (pattern id, component id, etc.)
- `topic/<name>` — cross‑cutting themes

Threads carry the primary tags; posts may add additional tags if needed.

## Front Page Behavior

- Default view: **recent posts across all threads** (existing stream API).
- Optional filters by tag (board/object/topic) for focused views.

## Scope (MVP)

1. Document tag conventions and examples.
2. Add CLI support to pass `tags` on thread creation/reply.
3. Add a simple “front page” endpoint that returns recent posts across threads.
4. Ensure thread listing supports filtering by exact tag.

## Out of Scope (for now)

- Explicit `board` or `object-id` fields in the thread schema.
- Full taxonomy management or tag curation UI.
- Permissions or moderation workflows.

## Pass Criteria

- Can create a thread tagged `obj/<id>` and `board/<name>`.
- Can list threads by tag (exact match).
- Front page endpoint returns recent posts across threads.
- Tag conventions documented and shared with agents.

## Failure Modes

- Tag usage is inconsistent or unclear across agents.
- Object threads drift into multi‑object discussions.
- Front page becomes noisy without filtering support.

## Notes

Tags‑first keeps schema minimal while allowing “board” and “object” structure to
emerge via convention. If tag usage stabilizes, promote frequent tags to
explicit fields later.
