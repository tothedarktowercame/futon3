; The Holistic Argument — machine-readable companion to holistic-argument.md
; Supersedes: futon3/holes/holistic-argument-sketch.sexp
; Updated: 2026-03-15 (post M-aif-head, three-pillar framing)

(holistic-argument

  (thesis
    "A software system can participate in its own maintenance if it keeps
     track of what it believes (the Argument), what it's sure of (the
     Invariants), and what it's doing about it (the Missions).")

  ;; ═══════════════════════════════════════════════════════════════════
  ;; The three pillars
  ;; ═══════════════════════════════════════════════════════════════════

  (pillars
    (pillar :id argument
      :aif-role :generative-model
      :description "Falsifiable model of what the system is and why it should exist"
      :artifacts ("docs/holistic-argument.md"
                  "docs/holistic-argument.sexp"
                  "docs/three-pillars.md")
      :support-claims (:S1 :S5))

    (pillar :id invariants
      :aif-role :precision
      :description "Machine-readable structural constraints classified by confidence"
      :artifacts ("docs/structural-law-inventory.sexp"
                  "docs/structural-crystallization.md"
                  "src/futon3c/logic/inventory.clj"
                  "src/futon3c/aif/invariant.clj")
      :support-claims (:S6))

    (pillar :id missions
      :aif-role :policy
      :description "Seven-phase process producing evidence at every step"
      :artifacts ("docs/futonic-missions.md"
                  "src/futon3c/peripheral/cycle.clj"
                  "src/futon3c/aif/mission_head.clj")
      :support-claims (:S2 :S3)))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Support claims
  ;; ═══════════════════════════════════════════════════════════════════

  (support-claims
    (claim :id S1
      :pillar :argument
      :text "The evidence discipline works"
      :evidence (853 :patterns
                 :psr-pur-records :active
                 :gate-enforcement :operational
                 :e2e-demo "M-futon3x-e2e"
                 :portfolio-inference "reads evidence counts for action selection"
                 :aif-heads "consult evidence landscape in observation loop"))

    (claim :id S2
      :pillar :missions
      :text "The Active Inference framing is generative"
      :evidence (:scales 4
                 :scale-1 "ant agents (futon2, 200-tick grid)"
                 :scale-2 "portfolio management (futon3c, 15 channels)"
                 :scale-3 "mission peripheral (futon3c, 10 channels, three-tier)"
                 :scale-4 "futonzero (futon0, capability observation, in-progress)"
                 :protocol "AifHead — composable 5-method interface"
                 :pattern-count 19 :pattern-subdirs 6
                 :reference "M-aif-head ARGUE phase, 19 patterns grounding 8 decisions"))

    (claim :id S3
      :pillar :missions
      :text "Pattern transfer is real"
      :evidence (:metaca-to-ants "wiring diagram patterns transfer between domains"
                 :math-informal "patterns apply to software reasoning"
                 :realtime "13 coordination patterns apply to any multi-agent system"
                 :mission-methodology "same 7 phases for AIF heads, Ramsey theory, documentation"))

    (claim :id S4
      :pillar nil ; cross-cutting
      :text "Commercial demand exists in adjacent spaces"
      :evidence (:landscape-intelligence "F7 lead report"
                 :markets ("knowledge graphs" "multi-agent coordination"
                           "pattern/rule systems" "formal methods")
                 :reference "M-f7-lead-report"))

    (claim :id S5
      :pillar :argument
      :text "The reflexive architecture is rare"
      :evidence (:three-pillars "docs/three-pillars.md — explains without AIF theory"
                 :self-verification "M-aif-head VERIFY applied AIF+ to its own structure"
                 :self-argument "this document is the system arguing for itself"))

    (claim :id S6
      :pillar :invariants
      :text "Structural constraints crystallize into enforceable law"
      :new true ; not in original sketch
      :evidence (:operational-families 9
                 :candidate-families 8
                 :check-law "validate-phase-advance consults AifHead.check-law"
                 :refusal "system can refuse Joe-initiated transitions"
                 :self-enforcing "C9 invariant enforcement produced by its own mission"
                 :reference "docs/structural-crystallization.md")))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Attack relations
  ;; ═══════════════════════════════════════════════════════════════════

  (attack-relations
    (attack :id A1
      :text "Complexity cost"
      :targets (:S1 :S6)
      :counter "Three-pillar structure manages complexity. Portfolio Inference
                ranks by EFE. CYDER (M-cyder) in progress. ~30 completed missions
                demonstrate finished work, not just complexity.")

    (attack :id A2
      :text "Solo-developer bottleneck"
      :targets (:S5)
      :counter "M-aif-head: Joe + claude-1 in 2 hours. Mission methodology makes
                agent contribution structurally possible via scoped handoffs.
                Bottleneck narrowing from 'only Joe' to 'Joe sets direction.'")

    (attack :id A3
      :text "Commercialisation gap"
      :targets (:S4)
      :counter "Landscape intelligence itself commercially viable. Pattern discipline
                + evidence architecture = enterprise developer tooling. Three-pillars
                doc first non-technical value proposition artifact.")

    (attack :id A4
      :text "Explanation problem"
      :targets (:S1 :S5)
      :counter "Three-pillars doc explains without AIF/CS jargon. Mission landscape
                appendix shows it's not theory. M-aif-head link provides public
                example. No longer a blank wall."))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Falsifiability conditions
  ;; ═══════════════════════════════════════════════════════════════════

  (falsifiability
    (condition :id F1
      :text "Evidence discipline produces no measurable benefit"
      :would-refute (:S1))
    (condition :id F2
      :text "Structural laws don't prevent real failures"
      :would-refute (:S6))
    (condition :id F3
      :text "The cycle doesn't close — evidence never feeds back"
      :would-refute (:S2 :S5))
    (condition :id F4
      :text "No one else can use it"
      :would-refute (:S5))
    (condition :id F5
      :text "Complexity grows faster than capability"
      :would-refute (:S1 :S6)))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; The generative cycle (step 5 is no longer missing)
  ;; ═══════════════════════════════════════════════════════════════════

  (generative-cycle
    (step 1 "Work produces proof"
      :mechanism "gate pipeline G5→G0, evidence emission at every phase")
    (step 2 "Proof produces patterns"
      :mechanism "L1 canonicalizer, Baldwin cycle, pattern library 853+")
    (step 3 "Patterns produce coordination"
      :mechanism "PSR before acting, PUR after, portfolio inference ranks by EFE")
    (step 4 "Coordination produces understanding"
      :mechanism "self-representing stack, three-column navigation, evidence viewer")
    (step 5 "Understanding produces argument"
      :mechanism "three-pillars doc, AIF heads (computable), VERIFY self-application"
      :formerly-missing true
      :closed-by ("docs/three-pillars.md"
                  "futon2/src/futon2/aif/head.clj"
                  "M-aif-head VERIFY phase")))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Devmap summaries (updated for the split)
  ;; ═══════════════════════════════════════════════════════════════════

  (devmaps
    (devmap :id futon0
      :summary "Human-system interface. Monitors vitality, rhythm, attention.
                Capability observation model for FutonZero."
      :status :operational)

    (devmap :id futon1a
      :summary "Deterministic storage. Datascript + XTDB, invariant enforcement,
                penholder auth, evidence persistence."
      :status :operational)

    (devmap :id futon2
      :summary "Active Inference engine. Ant demonstrators, AifAdapter protocol,
                FulabAdapter, AifHead protocol (new). The shared AIF substrate."
      :status :operational)

    (devmap :id futon3a
      :summary "Pattern guidance and audit. Portal (retrieval), Sidecar (audit trail),
                Compass (alignment scoring). The glacial timescale."
      :status :operational)

    (devmap :id futon3b
      :summary "Gate pipeline. G5→G0 quality assurance, task timescale coordination,
                L1 canonicalizer."
      :status :operational)

    (devmap :id futon3c
      :summary "Real-time coordination. Agency, peripherals, forum, drawbridge.
                The social timescale. Mission peripheral with AIF head. ~33 missions."
      :status :primary-development)

    (devmap :id futon4
      :summary "Memory workspace. Arxana hypertext, docbook, evidence viewer,
                self-representing stack, three-column invariants."
      :status :operational)

    (devmap :id futon5
      :summary "Wiring diagrams and design patterns. CT DSL, MetaCA, exotype space,
                composition validation."
      :status :operational)

    (devmap :id futon6
      :summary "Mathematics dictionary. Math-informal patterns, StackExchange corpus,
                FrontierMath pilot, rational reconstructions."
      :status :operational)

    (devmap :id futon7
      :summary "Knowledge economy. Landscape intelligence, commercial signal detection.
                Least developed — the strongest open framing gap."
      :status :nascent))

  ;; ═══════════════════════════════════════════════════════════════════
  ;; Counts snapshot (2026-03-15)
  ;; ═══════════════════════════════════════════════════════════════════

  (snapshot :date "2026-03-15"
    :missions-total 84
    :missions-complete 30
    :missions-in-progress 10
    :missions-ready 6
    :patterns 853
    :operational-invariant-families 9
    :candidate-invariant-families 8
    :test-assertions 4300
    :aif-head-implementations 1
    :repos 10))
