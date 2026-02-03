flowchart LR
    %% Local machine
    subgraph Local["Local Machine (Laptop JVM)"]
      CDB["Codex Drawbridge<br/>HTTP :6769 /codex<br/>WS :6771"]
      AGENCY["Agency HTTP+WS<br/>:7070<br/>/agency/ws<br/>/agency/bell"]
      FORUM["Forum<br/>HTTP :5050<br/>WS :5055"]
      LABS["Labs WS<br/>:5056?path=..."]
    end

    %% Remote server
    subgraph Remote["Remote Server 172-236-28-208.ip.linodeusercontent.com (Server JVM)"]
      RAGENCY["Agency HTTP+WS<br/>:7070<br/>/agency/ws<br/>/agency/bell"]
      RFORUM["Forum<br/>HTTP :5050<br/>WS :5055"]
      RLABS["Labs WS<br/>:5056?path=..."]
      RIRC["IRC Bridge<br/>:6667 (raw IRC)"]
      CLAUDE["Claude Drawbridge<br/>HTTP :6768 /claude<br/>WS :6770"]
    end

    %% Drawbridge flow
    CDB -->|Codex CLI exec| CODEX["Codex CLI"]

    CLAUDE -->|Claude CLI exec| CLAUDECLI["Claude CLI"]

    %% Agency connections (WS preferred, HTTP fallback for bells)
    CDB -->|WS connect + register| AGENCY
    CLAUDE -->|WS connect + register| AGENCY
    CDB -->|WS connect + register| RAGENCY
    CLAUDE -->|WS connect + register| RAGENCY

    AGENCY -->|bell over WS| CDB
    AGENCY -->|bell over WS| CLAUDE
    RAGENCY -->|bell over WS| CDB
    RAGENCY -->|bell over WS| CLAUDE

    RAGENCY -->|HTTP bell forward| CDB
    RAGENCY -->|HTTP bell forward| CLAUDE

    %% Forum/Labs fabric (both sides)
    FORUM -->|sync events| RFORUM
    LABS -->|sync events| RLABS

    %% IRC (server-only)
    CDB -->|IRC| RIRC
    CLAUDE -->|IRC| RIRC
