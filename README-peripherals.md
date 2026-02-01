# Peripherals: Minimal Contract + Routing Notes

This document defines a **minimal, machine-readable contract** for Futon3
peripherals so Agency can route summons across machines reliably (server ↔ laptop).

## What Is a Peripheral?

A **peripheral** is a *constrained capability envelope* for an agent.
It represents a specific task mode with explicit inputs, tools, and expected output,
so it can be invoked locally or routed remotely with predictable behavior.

Key properties:

- **Task-scoped**: designed for a specific intent (e.g., PAR contribution).
- **Explicit I/O**: declared inputs and normalized outputs.
- **Capability-bound**: tool access and context are constrained by spec.
- **Routable**: the same peripheral can run on server or laptop via Agency.
- **Composable**: peripherals can be chained or swapped without changing callers.

## Goals

- **Portable**: same peripheral can run locally or via remote Agency.
- **Predictable I/O**: clear inputs, outputs, and expected response shape.
- **Safe routing**: server Agency can delegate without ad-hoc prompt glue.

## Peripheral Spec (EDN)

Peripheral specs should live in `resources/agency/peripherals.edn`.

```clojure
{:id :par-participant
 :entry "Join PAR and contribute a concise perspective."
 :inputs [:par/title :par/crdt-host :par/crdt-port :session/id]
 :prompt-template "PAR: {{par/title}}\nCRDT: {{par/crdt-host}}:{{par/crdt-port}}\nSession: {{session/id}}\n\nInstructions: Provide 2-4 sentences per section you can answer."
 :tools {:fs false :shell false :http false}
 :response {:type :text :field :response}}
```

### Required Keys

- `:id` — unique peripheral identifier (keyword)
- `:entry` — short description of intent
- `:inputs` — list of expected input keys (namespaced keywords)
- `:prompt-template` — template used to build prompt at runtime
- `:response` — describes output shape (minimum is `{:type :text :field :response}`)

### Optional Keys

- `:tools` — capability envelope for the peripheral
- `:context` — list of context blocks to include (e.g., `:last-turns`, `:files`)
- `:timeout-ms` — override default timeout

## Agency Run Payload

Agency is invoked with a uniform payload so routing is transparent.

```json
{
  "agent-id": "fucodex",
  "peripheral": "par-participant",
  "inputs": {
    "par/title": "Lab Upload Debrief",
    "par/crdt-host": "172-236-28-208.ip.linodeusercontent.com",
    "par/crdt-port": 6530,
    "session/id": "019c16ac-..."
  }
}
```

### Inputs vs Prompt

`/agency/run` accepts either:

- `inputs` (preferred): values used to render `:prompt-template`
- `prompt`: raw user prompt (used when no structured inputs are available)

If both are provided, `inputs` populate the template and `prompt` is appended as
the "User request" block.

### Expected Response

```json
{
  "ok": true,
  "response": "...",
  "meta": {
    "agent-id": "fucodex",
    "duration-ms": 3214
  }
}
```

## Routing: Server ↔ Laptop

The server Agency owns **routing decisions**. A simple registry (EDN) maps agent
IDs to their location.

Example `resources/agency/agents.edn`:

```clojure
{:fucodex {:location :remote :url "http://LAPTOP_HOST:7070"}
 :fuclaude {:location :local}}
```

Routing rule:

- `:local` → handle via local `/agency/run`
- `:remote` → POST the same payload to remote Agency `/agency/run`

Responses are normalized so callers don’t care where execution happened.

## par-bell Integration (Agency-only)

`par-bell.sh` should call server Agency for each agent with:

- `peripheral: "par-participant"`
- inputs: PAR title, CRDT host/port, session id

No forum snapshot/merge is required for the minimal viable path.

## Future Extensions

- **Fanout**: dispatch multiple agents in parallel with aggregate responses.
- **Routing overrides**: allow per-request `:route` for ad-hoc targeting.
- **Context fetchers**: e.g. auto-inject last N turns or lab session context.
- **Timeout envelopes**: per-peripheral timeouts and retry policies.
