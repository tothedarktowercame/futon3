;; futon3/holes/excursions/E-clause-vocabulary-reshape.sexp
;;
;; Reshape rules for reflowing non-standard clause vocabulary into the
;; seven-component standard. Each rule is a small theorem about how a
;; non-canonical clause can be expressed inside the standard frame
;; without losing meaning.
;;
;; Companions:
;;   futon3/holes/excursions/E-clause-vocabulary-survey.md   ; evidence
;;   futon3/holes/excursions/data/clause-vocabulary-tally-2026-05-04.json
;;   futon3/holes/excursions/E-pattern-peripheral.md         ; enforcer
;;
;; Style-guide intent: future agents authoring patterns should know
;; (a) the seven canonical components, (b) how to add substructure
;; under them when more nuance is needed, (c) when to promote a
;; one-off to @-prefixed metadata, and (d) which old-style clauses
;; map to which new-style location.
;;
;; The reshape verb taxonomy:
;;   :replace      — the clause's content folds into one or more
;;                   canonical components; the old name disappears.
;;   :substructure — the clause survives as a named sub-marker under
;;                   a canonical component (e.g., +THEN > +CHECK).
;;                   Substructure preserves authorial nuance without
;;                   inventing a new top-level component.
;;   :metadata     — the clause is structurally metadata (status,
;;                   provenance, references), not pattern-content.
;;                   Promote to @-prefixed header.
;;   :facet        — a named cluster of substructure clauses that
;;                   express a single domain's vocabulary (e.g., the
;;                   meditation-pattern facet). Lives under a
;;                   canonical component as :facet/<name>.
;;   :reject       — the clause is genuinely orphaned and its content
;;                   distributes elsewhere; if no destination is
;;                   honest, the pattern itself needs rewriting.

{:reshape-spec/version           "v0-2026-05-04"
 :reshape-spec/ground-truth      "futon3/holes/excursions/E-clause-vocabulary-survey.md"
 :reshape-spec/raw-data          "futon3/holes/excursions/data/clause-vocabulary-tally-2026-05-04.json"
 :reshape-spec/canonical-order   [:context :if :however :then :because :next-steps]
 :reshape-spec/conclusion-marker :conclusion          ; the single ! line at top
 :reshape-spec/at-metadata-keys  #{:flexiarg :title :sigils :keywords
                                   :audience :tone :factor :style
                                   :references}        ; existing
                                   ;; new keys introduced by this spec
                                   ;; are added below per-rule.

 ;; ---------------------------------------------------------------------------
 ;; Universal canonical core — the seven clauses every pattern uses.
 ;; Survey: ≥86 % of files for each. No reshape; documented for completeness.
 ;; ---------------------------------------------------------------------------
 :reshape-spec/canonical
 {:conclusion {:role  "top-level claim of the pattern"
               :form  "! conclusion: <one-paragraph claim>"
               :aliases  ;; Syntactic sugar — all four normalise to :conclusion
                         ;; in `:pattern/conclusion` / `:pattern/has-conclusion`.
                 #{:conclusion :claim :summary :instantiated-by}
               :required? true   ;; the only mandatory canonical slot;
                                  ;; verified by
                                  ;; futon1a invariant-pattern-has-conclusion
               :note  "Authorial declaration. Folds in INVARIANT-STATEMENT
                       when present; the conclusion IS the invariant.
                       `! summary:` and `! instantiated-by:` are
                       sugar (devmap-prototype headings use the
                       latter); `! claim:` is the historical Toulmin
                       form. The canonical parser
                       (futon3a/src/futon/flexiarg/projection.clj)
                       and the futon1a ingest both normalise the
                       four to the same conclusion slot."}
  :context    {:role  "the pattern's domain of discourse"
               :form  "+ context: <prose>"
               :note  "What world does this pattern live in?"}
  :if         {:role  "scope precondition"
               :form  "+ IF: <prose>"
               :note  "When the pattern's claim holds. Folds in APPLIES-WHEN."}
  :however    {:role  "the structural tension — what the pattern's claim
                       resists or does not yet honour"
               :form  "+ HOWEVER: <prose>"
               :note  "Together with IF, defines the pattern's TENSION
                       constructively. No separate :tension clause is
                       needed (rule :tension below)."}
  :then       {:role  "what the pattern resolves the tension into"
               :form  "+ THEN: <prose>"
               :note  "The constructive move. Sub-structure (CHECK,
                       COMPOSITIONS, SIGNALS, ...) lives below."}
  :because    {:role  "the warrant — why the pattern's move resolves the
                       tension"
               :form  "+ BECAUSE: <prose>"
               :note  "Substructure (EVIDENCE, MECHANISM, COUNTERFACTUAL)
                       lives below."}
  :next-steps {:role  "operational follow-on"
               :form  "+ NEXT-STEPS: <list>"}}

 ;; ---------------------------------------------------------------------------
 ;; Reshape rules — non-canonical clause → canonical placement.
 ;; Ordered roughly by survey frequency, most common first.
 ;; ---------------------------------------------------------------------------
 :reshape-spec/rules
 [;; ===== Family: structural-tension =====================================
  {:clause     :tension
   :family     :structural-tension
   :occurrences 63 :files 63
   :reshape    :replace
   :replace-with [:if :however]
   :rule       "Split into IF (what should hold) + HOWEVER (what fails to)."
   :rationale  "The structural tension IS the gap between IF and HOWEVER;
                an explicit :tension clause is a redundant labeling of
                that gap."
   :higher-d-note
                "Higher-dimensional / hypergraphical tensions still
                 describable: each axis composes a conjunction inside IF
                 and HOWEVER, OR the pattern factors into multiple
                 canonical pairs (cf. CONJUNCTION below)."
   :examples   ["library/ukrns/*.flexiarg"
                "library/writing-coherence/*.flexiarg"
                "library/peeragogy/*.flexiarg"
                "library/collaboration-coherence/*.flexiarg"
                "library/math-formalization/*.flexiarg"]}

  {:clause     :compositions
   :family     :structural-tension
   :occurrences 60 :files 60
   :reshape    :substructure
   :under      :then
   :form       "+ THEN: ...\n  + COMPOSITIONS: <prose>"
   :rule       "Keep the COMPOSITIONS marker as a sub-clause of THEN."
   :rationale  "Composition with siblings is part of the pattern's
                resolution-shape, not a separate structural component.
                Joe's example (+THEN > +COMPOSITIONS +CHECK) is the
                canonical form."
   :examples   ["library/ukrns/*.flexiarg"
                "library/writing-coherence/*.flexiarg"]}

  {:clause     :check
   :family     :structural-tension
   :occurrences 63 :files 63
   :reshape    :substructure
   :under      :then
   :form       "+ THEN: ...\n  + CHECK: <verifiable predicate>"
   :rule       "Keep CHECK as a sub-clause of THEN."
   :rationale  "A check is a verifier on the THEN claim. It belongs
                under THEN as the operational test that the move
                actually resolved the tension."}

  {:clause     :failure-modes
   :family     :universal-tolerated
   :occurrences 94 :files 94
   :reshape    :substructure
   :under      :however
   :form       "+ HOWEVER: ...\n  + FAILURE-MODES: <enumerated list>"
   :rule       "Keep FAILURE-MODES as a sub-clause of HOWEVER."
   :rationale  "Failure modes are concrete instantiations of the
                structural tension. They sharpen HOWEVER without
                competing with it."}

  ;; ===== Family: invariant-shape =========================================
  {:clause     :invariant-statement
   :family     :invariant-shape
   :occurrences 56 :files 56
   :reshape    :replace
   :replace-with [:conclusion]
   :rule       "Fold into the ! conclusion: line."
   :rationale  "An invariant pattern's invariant IS its conclusion. A
                separate :invariant-statement clause duplicates the
                top-level claim; the reshape script lifts it into
                ! conclusion: when ! conclusion: is missing, otherwise
                merges them."
   :examples   ["library/futon-theory/*.flexiarg"
                "library/invariant-coherence/*.flexiarg"
                "library/equity/*.flexiarg"]}

  {:clause     :anti-patterns
   :family     :invariant-shape
   :occurrences 29 :files 29
   :reshape    :substructure
   :under      :however
   :form       "+ HOWEVER: ...\n  + ANTI-PATTERNS: <enumerated list>"
   :rule       "Keep ANTI-PATTERNS as a sub-clause of HOWEVER."
   :rationale  "Anti-patterns are named instantiations of the tension —
                they describe specific failure-shapes the canonical
                move must avoid. Same structural slot as :failure-modes."}

  {:clause     :ancestors
   :family     :invariant-shape
   :occurrences 35 :files 35
   :reshape    :metadata
   :as         :@ancestors
   :form       "@ancestors [<pattern-ref> ...]"
   :rule       "Promote to a top-level @-prefixed metadata list."
   :rationale  "Ancestor pointers are pattern-graph metadata, not
                pattern content. They should live alongside @references,
                not inside the clause body. Reshape merges :ancestors
                into the existing @references list when possible
                (@references already exists in 16 % of patterns)."}

  {:clause     :related-patterns
   :family     :invariant-shape
   :occurrences 32 :files 32
   :reshape    :metadata
   :as         :@references
   :form       "@references [<pattern-ref> ...]"
   :rule       "Merge into @references."
   :rationale  "Identical semantics to @references. The reshape just
                relocates."}

  {:clause     :derivation
   :family     :invariant-shape
   :occurrences 39 :files 39
   :reshape    :replace
   :replace-with [:because]
   :rule       "Fold into BECAUSE."
   :rationale  "A derivation IS the warrant — it shows why the
                conclusion follows. BECAUSE is the canonical home for
                that role."}

  {:clause     :enforcement
   :family     :invariant-shape
   :occurrences 8 :files 8
   :reshape    :substructure
   :under      :then
   :form       "+ THEN: ...\n  + ENFORCEMENT: <how the invariant binds>"
   :rule       "Substructure under THEN."
   :rationale  "Enforcement describes how the THEN move binds —
                identical structural slot as CHECK."}

  ;; ===== Family: causal-mechanism (t3 PL formalism) =====================
  ;; Per Joe: t3 is a genuine PL-formalism effort. Reshape preserves the
  ;; PL formalism as substructure under the standard components, so the
  ;; formalism is still expressible while the canonical frame holds.
  {:clause     :mechanism
   :family     :causal-mechanism
   :occurrences 13 :files 13
   :reshape    :substructure
   :under      :because
   :form       "+ BECAUSE: ...\n  + MECHANISM: <causal account>"
   :rule       "Substructure under BECAUSE."
   :rationale  "The mechanism is the warrant's content — what makes
                the pattern's move actually work. Lives under BECAUSE."}

  {:clause     :counterfactual
   :family     :causal-mechanism
   :occurrences 12 :files 12
   :reshape    :substructure
   :under      :because
   :form       "+ BECAUSE: ...\n  + COUNTERFACTUAL: <not-X account>"
   :rule       "Substructure under BECAUSE."
   :rationale  "Counterfactuals strengthen the warrant by showing what
                fails when the pattern is absent. Same slot as MECHANISM."}

  {:clause     :applies-when
   :family     :causal-mechanism
   :occurrences 12 :files 12
   :reshape    :replace
   :replace-with [:if]
   :rule       "Fold into IF."
   :rationale  "\"applies when X\" is literally \"if X\". Identical
                semantics."}

  {:clause     :does-not-apply
   :family     :causal-mechanism
   :occurrences 12 :files 12
   :reshape    :substructure
   :under      :if
   :form       "+ IF: ...\n  + DOES-NOT-APPLY: <out-of-scope cases>"
   :rule       "Substructure under IF."
   :rationale  "Negative scoping is the IF clause's complement —
                belongs under it as a named exclusion list."}

  {:clause     :signals
   :family     :causal-mechanism
   :occurrences 12 :files 12
   :reshape    :substructure
   :under-when {:check-exists :check        ; THEN > CHECK > SIGNALS
                :otherwise    :however}     ; HOWEVER > SIGNALS (parallel to ABSENCE-SIGNALS)
   :form       "if THEN+CHECK both exist:
                    + THEN: ...\n  + CHECK: ...\n    + SIGNALS: <confirmation indicators>
                otherwise (t3-style violation-indicator usage):
                    + HOWEVER: ...\n  + SIGNALS: <violation indicators>"
   :rule       "Heuristic placement: when CHECK exists (move-confirmation
                shape) signals goes under it as the empirical surface;
                when CHECK is absent (violation-indicator shape, common
                in t3) signals goes under HOWEVER as a sibling of
                ABSENCE-SIGNALS / FAILURE-MODES."
   :rationale  "Two distinct semantics for `:signals` are visible in the
                corpus: (a) confirmation signals — observable indicators
                that the THEN move is working; (b) violation signals —
                observable indicators that the pattern is being violated
                or the tension is manifesting. The structural-tension
                family uses CHECK for (a); the t3 family uses :signals
                in mode (b) (\"Trainers report isolation,\" etc.). Routing
                by presence-of-CHECK preserves both semantics without
                synthesizing placeholder CHECK stubs in patterns whose
                signals are violation-shape."
   :amendment  "Updated 2026-05-04 after first reshape pass on t3/
                surfaced 12 files where CHECK did not exist; rulebook
                originally specified depth-2 substructure (THEN > CHECK
                > SIGNALS) which would have required synthesizing CHECK
                stubs for those files."}

  {:clause     :absence-signals
   :family     :causal-mechanism
   :occurrences 16 :files 16
   :reshape    :substructure
   :under      :however
   :form       "+ HOWEVER: ...\n  + ABSENCE-SIGNALS: <indicators that the pattern is being violated>"
   :rule       "Substructure under HOWEVER."
   :rationale  "Absence-signals are observable indicators of the
                tension actively manifesting. Symmetric to SIGNALS
                under THEN > CHECK; lives under HOWEVER."}

  ;; ===== Family: evidence-warrant ========================================
  {:clause     :evidence
   :family     :universal-tolerated
   :occurrences 165 :files 147
   :reshape    :substructure
   :under      :because
   :form       "+ BECAUSE: ...\n  + EVIDENCE: <empirical backing>"
   :rule       "Substructure under BECAUSE."
   :rationale  "Evidence is the warrant's empirical content. BECAUSE
                supplies the argument; EVIDENCE supplies the data
                the argument leans on."}

  {:clause     :because->evidence
   :family     :evidence-warrant
   :occurrences 28 :files 12
   :reshape    :replace
   :replace-with [:because]   ; with EVIDENCE substructure
   :rule       "Treat as a stylistic shorthand for BECAUSE with a
                pointer to EVIDENCE substructure; emit BECAUSE +
                EVIDENCE pair."
   :rationale  "Same semantics as :evidence under :because; the arrow
                in the clause-name is just authorial flourish."}

  {:clause     :evidence-base
   :family     :evidence-warrant
   :occurrences 32 :files 4
   :reshape    :replace
   :replace-with [:because]   ; with EVIDENCE substructure
   :rule       "Same as :evidence."
   :rationale  "Synonym for :evidence in p4ng patterns."}

  ;; ===== Family: lifecycle / metadata-shaped =============================
  {:clause     :status
   :family     :lifecycle-metadata
   :occurrences 109 :files 68
   :reshape    :metadata
   :as         :@status
   :form       "@status <draft|operational|deprecated|...>"
   :rule       "Promote to @-prefixed metadata."
   :rationale  "Status is pattern-state, not pattern-content. It
                belongs alongside @sigils and @audience."}

  {:clause     :governance
   :family     :lifecycle-metadata
   :occurrences 41 :files 16
   :reshape    :metadata
   :as         :@governance
   :form       "@governance <owner-or-policy-pointer>"
   :rule       "Promote to @-prefixed metadata."}

  {:clause     :instantiated-by
   :family     :lifecycle-metadata
   :occurrences 64 :files 9
   :reshape    :metadata
   :as         :@instantiated-by
   :form       "@instantiated-by [<devmap-prototype-ref> ...]"
   :rule       "Promote to @-prefixed metadata."
   :rationale  "Devmap-prototype linkage is a graph-edge to the
                holistic-argument's prototype level; metadata, not
                clause content."}

  {:clause     :illustrates
   :family     :lifecycle-metadata
   :occurrences 23 :files 6
   :reshape    :metadata
   :as         :@illustrates
   :form       "@illustrates [<pattern-or-mission-ref> ...]"
   :rule       "Promote to @-prefixed metadata."}

  ;; ===== Family: connectors / synonyms ==================================
  {:clause     :therefore
   :family     :connectors
   :occurrences 38 :files 8
   :reshape    :replace
   :replace-with [:then]
   :rule       "Fold into THEN."
   :rationale  "Logical-connector phrasing of the same slot."}

  {:clause     :use
   :family     :connectors
   :occurrences 12 :files 10
   :reshape    :substructure
   :under      :next-steps
   :form       "+ NEXT-STEPS: ...\n  + USE: <how to operationalise>"
   :rule       "Substructure under NEXT-STEPS."
   :rationale  "USE specifies how an operator deploys the pattern —
                same time-horizon as NEXT-STEPS, sharper authorial
                focus."}

  ;; ===== Facet: math-formalization (Lean) ===============================
  {:clause     :lean
   :family     :math-formalization
   :occurrences 15 :files 15
   :reshape    :substructure
   :under      :then
   :form       "+ THEN: ...\n  + LEAN: <Lean snippet or pointer>"
   :rule       "Substructure under THEN."
   :rationale  "Lean code is the formal expression of the THEN move
                in math-formalization patterns. Substructure preserves
                the snippet without reinventing a top-level slot."
   :alt-rule   "If a pattern carries multiple formalisations
                (Lean + Coq + Isabelle), promote to a :facet/formal
                cluster under THEN; see :facet/formal below."}

  ;; ===== Facet: meditation-patterns (liberation namespace) ==============
  ;; Joe's framing: \"if they are meaningful within the pattern language
  ;; facet, they could be included in the at-prefixed metadata.\"
  ;; Treatment: cluster as a named facet under CONTEXT (these clauses
  ;; describe the pattern's framing layers, not its tension/move).
  ;; The facet name @meditation makes the cluster legible as a
  ;; coherent vocabulary.
  {:facet      :meditation
   :family     :liberation
   :clauses    [:mundane-content :ripens-in :transcendent-quality
                :distinguishing-from-mundane :worn-away
                :the-running-circle]
   :occurrences-total 47
   :reshape    :facet
   :under      :context
   :form       "+ context: ...\n  + MEDITATION/MUNDANE-CONTENT: ...\n  + MEDITATION/RIPENS-IN: ...\n  + MEDITATION/TRANSCENDENT-QUALITY: ..."
   :alt-form   "@meditation { :mundane-content \"...\"
                              :ripens-in \"...\"
                              :transcendent-quality \"...\" }"
   :rule       "Cluster as a named facet under CONTEXT, OR promote
                to @meditation metadata block when the pattern's
                primary content is the facet itself."
   :rationale  "These clauses are a coherent domain vocabulary for
                Buddhist-meditation patterns — not random one-offs.
                Preserving them as a named facet keeps the pattern-
                language facet legible while the canonical seven
                still scaffold the pattern's argument."}

  ;; ===== Facet: 8FP (eight-fold-path patterns) ==========================
  {:facet      :8fp
   :family     :eight-fold-path
   :clauses    [:distinguishing-from-wrong-view
                :distinguishing-from-wrong-mindfulness
                :distinguishing-from-wrong-livelihood
                :distinguishing-from-wrong-concentration
                :distinguishing-from-wrong-speech
                :distinguishing-from-wrong-action
                :distinguishing-from-wrong-effort
                :distinguishing-from-wrong-thought]
   :occurrences-total 8
   :reshape    :facet
   :under      :however
   :form       "+ HOWEVER: ...\n  + 8FP/DISTINGUISHING-FROM-WRONG-<axis>: <prose>"
   :alt-form   "@8fp { :distinguishing-from-wrong-view \"...\" ... }"
   :rule       "Cluster as @8fp facet under HOWEVER."
   :rationale  "Each \"distinguishing-from-wrong-X\" clause is the 8FP
                pattern's HOWEVER axis — it names what the right-X
                pattern is NOT. Cluster preserves the 8-axis vocabulary
                without forcing eight separate top-level clauses."}

  ;; ===== Long-tail catch-all ============================================
  ;; Updated 2026-05-04: as the operator-by-proxy pass, the script now
  ;; routes long-tail bespoke clauses by name-keyword heuristic rather
  ;; than emitting per-pattern review records. The heuristic is below.
  ;; The original :inspect verb and rationale are preserved in the
  ;; :alt-rule field for future reviews where a per-pattern operator
  ;; pass IS the right move.
  {:clause     :*
   :family     :long-tail-bespoke
   :occurrences-total 518
   :distinct-keys 223
   :reshape    :heuristic-classify
   :rule       "Route by name-keyword to the canonical component whose
                semantic role best fits, preserving the original clause
                as named substructure. The classify table below is the
                live operator-by-proxy default; the rulebook captures it
                so future agents see what was decided and can revise
                per-pattern if the heuristic was wrong."
   :alt-rule   "Per-pattern operator review (the original :inspect
                verb). Use when the heuristic produces a structurally
                wrong placement; record the override in
                holes/excursions/data/clause-reshape-decisions.edn."

   :heuristic-classify
   ;; Each entry: name-keyword pattern → canonical destination.
   ;; Patterns are matched by substring; first match wins.
   {:however
      ["violation" "wrong-" "ghost-" "instability" "failure"
       "worn-away" "running-circle" "absence" "anti-" "tripwire"
       "broken"]
    :because
      ["evidence" "mechanism" "counterfactual" "warrant"
       "derivation" "rationale" "gain-calculation" "because"]
    :if
      ["requirements" "minimum-viable" "applies-when" "agent-types"
       "contract-requirements" "not-required"]
    :next-steps
      ["escalation" "reversal" "next-" "follow-on"]
    :context  ; 8FP/meditation framing layers
      ["mundane-content" "transcendent-quality" "ripens-in"
       "distinguishing-from-mundane" "distinguishing-from-wrong"]
    :then     ; default — implementation / procedure / move
      ["*"]}

   :metadata-classify
   ;; Names that are structurally metadata, not pattern content.
   {:references
      ["relationship-to-" "relation-to-" "related"]
    :definition  ["definition"]
    :specifies   ["specifies"]
    :layer-position ["layer-position"]}

   :rationale  "The long tail is mostly intentional, not accidental
                (survey F-4). The heuristic preserves the authorial
                clause name as named substructure (no information
                lost) while putting it under the canonical component
                that matches its semantic shape. A future operator
                pass can refine: re-classify to a different parent if
                the heuristic was wrong, or promote to @-metadata if
                the clause is structurally meta. The 8FP/meditation
                facets get explicit treatment via the CONTEXT routing
                — the heuristic was tuned against the survey's worked
                examples, then sanity-checked across the library."}]

 ;; ---------------------------------------------------------------------------
 ;; Worked example — t3/pull-back-the-tape.flexiarg style.
 ;; Joe's note: t3 is a genuine PL-formalism effort. Reshape preserves
 ;; the formalism as substructure; nothing is lost.
 ;; ---------------------------------------------------------------------------
 :reshape-spec/worked-example/t3
 {:before
  "+ MECHANISM: <causal account of how X produces Y>
   + COUNTERFACTUAL: <what fails if X is absent>
   + APPLIES-WHEN: <scope predicate>
   + DOES-NOT-APPLY: <out-of-scope cases>
   + SIGNALS: <observable indicators>"
  :after
  "+ IF: <scope predicate>
     + DOES-NOT-APPLY: <out-of-scope cases>
   + HOWEVER: <structural-tension prose>
   + THEN: <move>
     + CHECK: <verifiable predicate>
       + SIGNALS: <observable indicators>
   + BECAUSE: <warrant>
     + MECHANISM: <causal account of how X produces Y>
     + COUNTERFACTUAL: <what fails if X is absent>"
  :note
  "The PL formalism survives intact — every t3 clause has a home.
   What changes: the canonical seven scaffold the argument, so the
   pattern is now legible to retrieval / HIT / typed-slot lift the
   same way as any other library pattern."}

 ;; ---------------------------------------------------------------------------
 ;; Reshape execution plan (informational; the actual reshape is a
 ;; future probe, scoped after P-1 lands the canonical parser).
 ;; ---------------------------------------------------------------------------
 :reshape-spec/execution-plan
 {:phase-1
  {:name        "Reshape script (read-only dry-run)"
   :description "Walk every .flexiarg, apply rules above, emit a
                 unified-diff per pattern showing what would change.
                 Produces a review artifact without modifying disk."
   :depends-on  [:p-1-canonical-parser]
   :output      "futon3/holes/excursions/data/clause-reshape-dryrun-<date>.diff"}
  :phase-2
  {:name        "Operator review of long-tail (rule :*)"
   :description "For each :inspect pattern, operator decides:
                 (a) attach as substructure, (b) promote to metadata,
                 (c) reject. Decisions land in
                 futon3/holes/excursions/data/clause-reshape-decisions.edn."
   :depends-on  [:phase-1]}
  :phase-3
  {:name        "Apply reshape to disk"
   :description "Apply the dry-run + decisions; commit per-namespace
                 (one block-id per namespace, per
                 library/structure/block-as-futonic-revolution.flexiarg).
                 Verify deterministic round-trip via the canonical
                 parser."
   :depends-on  [:phase-2 :p-1-canonical-parser]}
  :phase-4
  {:name        "Lock vocabulary via Pattern Peripheral phase-2"
   :description "Once the corpus is uniformly reshaped, the peripheral
                 flips from advisory to enforced for the canonical
                 seven + universal-tolerated. Per-namespace canonical
                 sets remain advisory until per-family curation."
   :depends-on  [:phase-3 :pattern-peripheral-phase-2]}}

 ;; ---------------------------------------------------------------------------
 ;; Open questions (carry forward when execution starts)
 ;; ---------------------------------------------------------------------------
 :reshape-spec/open-questions
 [{:q  "Should @ancestors merge with @references or remain a
        separate top-level metadata key?"
   :note "Ancestors imply genealogy (this pattern descends from X);
          references imply citation (this pattern cites X). Different
          relations. Recommendation: keep @ancestors separate; the
          reshape script does not silently merge."}

  {:q  "How do facets compose? A pattern in :liberation that also
        carries a Lean formalism would have both :facet/meditation
        under CONTEXT and :facet/formal under THEN. Is multi-facet
        composition explicit or just \"obviously fine\"?"
   :note "Recommend: obviously fine. Each facet lives under exactly
          one canonical component; composition across components is
          how the canonical seven were designed."}

  {:q  "When a reshape collapses two old clauses into one canonical
        component (e.g., :tension → :if + :however), the prose may
        need authorial merging, not just relocation. Is the reshape
        script allowed to LLM-merge, or must operator review every
        such case?"
   :note "Recommend: dry-run emits the structural reshape plus a
          'requires-prose-merge' flag for cases where the original
          :tension prose maps to BOTH :if and :however authorially.
          Operator decides per-case. LLM-merge is phase-2 of the
          reshape itself, gated on operator-approved exemplars."}]}
