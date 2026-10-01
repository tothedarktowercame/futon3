(ns futon3.turnfeed.core
  "One page holding every operator turn with its annotations inline.

  The margin pages hand the reader a link per turn. This hands them the
  turns: text in a reading measure, the interpretation beside it, newest
  first, refreshed in place as analyses land. Nothing to click through."
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom-client]
            [clojure.string :as str]))

(defonce state
  (r/atom {:turns [] :fetched-at nil :error nil
           :lit nil          ; the note id a cue is pointing at
           :filter ""}))

;; kimi-2's alignment of the intent vocabulary onto ChipWits IBOL chips
;; (2026-09-23). The chips are 1-bit black on transparent and sit on the
;; #fffff8 page without a plate behind them. An intent with no chip simply
;; shows its name -- a missing icon is not worth a wrong one.
(def intent-chip
  {"ask-action" "op-go"           "explain"     "op-sing"
   "report"     "op-sing"         "clarify"     "op-look-for"
   "propose"    "op-subpanel"     "constrain"   "arg-wall"
   "unresolved" "op-flip-coin"    "qualify"     "op-num-equal"
   "report-problem" "arg-bomb"    "approve"     "op-plus"
   "disagree"   "op-minus"        "extend"      "op-wire"
   "prioritize" "arg-num-stack"   "defer"       "op-keypress"
   "continue"   "arg-forward"     "delegate"    "op-boomerang"
   "redirect"   "arg-turn-right"  "collect"     "op-pickup"
   ;; verify has no chip in kimi-2's table, and the extracted set has no
   ;; op-test. op-qray is the query ray -- scan a square to learn what is
   ;; actually there, which is the move.
   "verify"     "op-qray"
   ;; Joe's choices, 2026-10-01.
   "explore"    "op-move"         "withdraw"    "op-door"
   "gist"       "op-loop"})

(defn chip [intent]
  (when-let [c (intent-chip intent)]
    [:img.chip {:src (str "chips/" c ".png") :alt "" :title (str intent " · " c)}]))

;; --- the IBOL legend ----------------------------------------------------------
;; What each chip means on this page, read three ways: the ChipWits operator
;; (the 1984 manual, chipwits-forth/docs/ChipWits_Mac_Manual.pdf), the intent
;; label the annotator gives a span of Joe's turn, and where that turn lands in
;; the agent's perception-action loop. The stages and R-nodes are the War
;; Machine catalogue's (p4ng/empirics-futon/control-stages.edn); the IBOL-as-AIF
;; readings follow futon2 holes/labs/wm-contract/NOTE-ibol-to-aif.md where it
;; has one. Which R-node each intent lands on is claude-12's proposal
;; (2026-09-25), not a measured fact: the test is whether turns read this way
;; predict what the agent did next.

;; The mark each intent carries in agent replies (~/code/CLAUDE.md, "Reply
;; proforma") and in 小象 (futon3c/emacs/xiaoxiang-preview.el, whose
;; `xiaoxiang-preview-marks' must agree with this map). With legend-rows this
;; makes one table: intent, mark, chip, loop stage, R-node (Joe, 2026-10-01).
(def intent-mark
  {"gist" "㊥" "propose" "㊭" "approve" "㊣" "disagree" "🈚" "qualify" "㊟"
   "explain" "🈖" "clarify" "🈯" "report" "㊢" "report-problem" "㊩"
   "verify" "㊬" "retract" "🈹" "withdraw" "🈡" "unresolved" "🈳"
   "constrain" "🈲" "ask-action" "🈸" "delegate" "㊯" "prioritize" "㊝"
   "collect" "㊮" "extend" "🈕" "continue" "🈰" "defer" "🈝"
   "redirect" "🈘" "explore" "㊫"})

(def loop-stages
  [["PERCEIVE" "what the agent observes"]
   ["BELIEVE"  "what it takes to be the case, and how sure it is"]
   ["EVALUATE" "how it scores what could happen"]
   ["SELECT"   "which course it commits to, and over what horizon"]
   ["ACT"      "what it does, and who certifies it"]
   ["ANNOTATOR" "not the agent's loop: the reading of the turn itself"]])

(def legend-rows
  [{:stage "PERCEIVE" :chip "arg-bomb" :ibol "BOMB (a Thing)"
    :ibol-says "a Thing that damages the robot if it is zapped or run into"
    :intents ["report-problem"] :r "R8 present-fit mismatch"
    :aif "Joe reports that something the agent produced does not fit the world: a prediction error delivered from outside. It is a hit on the damage meter, not a new goal."}
   {:stage "PERCEIVE" :chip "op-sing" :ibol "SING"
    :ibol-says "sing a note; the manual's only non-behavioural output"
    :intents ["explain" "report"] :r "R2 structured observation"
    :aif "Joe supplies context the agent could not observe for itself. Evidence for the agent, though the chip is the robot's own voice: the note on SING (self-report is not evidence) applies to the agent singing, not to Joe."}
   {:stage "BELIEVE" :chip "op-look-for" :ibol "LOOK (for a Thing)"
    :ibol-says "look ahead for a named Thing; true wire if seen, false if not"
    :intents ["clarify"] :r "R7 evidence-channel precision"
    :aif "Joe sharpens what an earlier ask meant. The observation is the same; its precision goes up, so the agent should weight it more and its own guess less."}
   {:stage "BELIEVE" :chip "op-num-equal" :ibol "COMPARE NUMBER: EQUAL?"
    :ibol-says "test a value against a number; branch on the answer"
    :intents ["qualify"] :r "R3 belief update"
    :aif "Joe narrows a claim to where it holds. The belief is kept but its scope is cut."}
   {:stage "BELIEVE" :chip "op-plus" :ibol "INCREMENT"
    :ibol-says "add one to the top of the number stack"
    :intents ["approve"] :r "R3 belief update"
    :aif "Positive evidence on the agent's last step. In the IBOL note this is fuel: operator attention restored."}
   {:stage "BELIEVE" :chip "op-minus" :ibol "DECREMENT"
    :ibol-says "subtract one from the top of the number stack"
    :intents ["disagree"] :r "R3 belief update"
    :aif "Negative evidence: Joe holds a different belief and says so. Unlike report-problem, the disagreement is about the model, not about an output."}
   {:stage "BELIEVE" :chip "op-pickup" :ibol "PICK UP"
    :ibol-says "take whatever Thing is directly ahead"
    :intents ["collect"] :r "R1 belief state"
    :aif "Joe asks for something to be gathered into the agent's working state. The IBOL note: you can only pick up what was observed adjacent."}
   {:stage "EVALUATE" :chip "arg-wall" :ibol "WALL (a Thing)"
    :ibol-says "part of the room the robot cannot pass"
    :intents ["constrain"] :r "R5 expected free energy (preferences)"
    :aif "Joe rules out a region of outcomes. In AIF terms it changes the preferences G is scored against; in the IBOL note a wall is an invariant or refusal."}
   {:stage "EVALUATE" :chip "op-wire" :ibol "WIRE"
    :ibol-says "the authored edge between two chips"
    :intents ["extend"] :r "R4 forward model"
    :aif "Joe adds a step or link the agent's model of consequences did not have: an authored edge in the cascade."}
   {:stage "SELECT" :chip "op-subpanel" :ibol "SUB-PANEL"
    :ibol-says "call one of the seven sub-programs, then continue"
    :intents ["propose"] :r "R6 candidate action space"
    :aif "Joe offers a course of action the agent may not have generated. It enters the candidate set; it is not yet chosen."}
   {:stage "SELECT" :chip "arg-num-stack" :ibol "NUMBER STACK"
    :ibol-says "the ordered stack of values the robot keeps"
    :intents ["prioritize"] :r "R14 commitment temperature"
    :aif "Joe orders the candidates. The agent should commit more sharply to the one ranked first."}
   {:stage "SELECT" :chip "arg-turn-right" :ibol "TURN (right)"
    :ibol-says "rotate the robot in place"
    :intents ["redirect"] :r "R15 hierarchy and timescale"
    :aif "The layer above overrides the current course. Joe acting as the slower, higher layer of a hierarchical model."}
   {:stage "SELECT" :chip "op-keypress" :ibol "KEYPRESS"
    :ibol-says "check whether the player pressed a key; the always-checked chip"
    :intents ["defer"] :r "R13 temporal policy depth"
    :aif "Joe moves something to later. The horizon changes; the item stays."}
   {:stage "SELECT" :chip "op-boomerang" :ibol "BOOMERANG"
    :ibol-says "return from a sub-panel to the main panel"
    :intents ["delegate"] :r "R11 hierarchical shared budget"
    :aif "Work is handed to another agent and comes back with a value. The budget is shared across the two."}
   {:stage "ACT" :chip "op-go" :ibol "GO"
    :ibol-says "the GO marker: where a panel starts executing"
    :intents ["ask-action"] :r "R16 grounded enactment"
    :aif "Joe asks the agent to do something in the world, not to say something about it."}
   {:stage "ACT" :chip "arg-forward" :ibol "MOVE (forward)"
    :ibol-says "one step ahead"
    :intents ["continue"] :r "R16 grounded enactment"
    :aif "Keep enacting the current course."}
   {:stage "ACT" :chip "op-qray" :ibol "Q-RAY"
    :ibol-says "scan a square to learn what is actually there"
    :intents ["verify"] :r "R9 no self-certification"
    :aif "Joe asks for a check the agent's own account cannot supply. An epistemic act, and the assurance node that an agent may not certify itself."}
   {:stage "ANNOTATOR" :chip "op-flip-coin" :ibol "COIN FLIP"
    :ibol-says "a random choice between the true and false wires"
    :intents ["unresolved"] :r "—"
    :aif "The annotator could not settle an intent. In the IBOL note a coin flip is a tie recorded in the open; here it marks a span left unread, not a move by Joe."}
   ;; The four intents below: stage and R-node are claude-17's proposal
   ;; (2026-10-01), on the same test as claude-12's. Chips are Joe's choice
   ;; (2026-10-01), cut from chipwits-forth mac/graphics/IBOL_Graphics.png;
   ;; their IBOL meanings are not yet checked against the manual.
   {:stage "BELIEVE" :chip nil :ibol "(no chip)" :ibol-says ""
    :intents ["retract"] :r "R3 belief update"
    :aif "Joe takes back something he said. An observation the agent had already used is removed, so beliefs built on it should be revised."}
   {:stage "EVALUATE" :chip "op-move" :ibol "ROLLER SKATE" :ibol-says ""
    :intents ["explore"] :r "R5 expected free energy (epistemic value)"
    :aif "Joe asks to find out rather than to get something done. In G this is the epistemic term: a course is worth taking for what it would reveal."}
   {:stage "SELECT" :chip "op-door" :ibol "DOOR" :ibol-says ""
    :intents ["withdraw"] :r "R6 candidate action space"
    :aif "Joe ends an earlier act of his, such as an offer or a commitment. A course that was available is taken out of the candidate set."}
   {:stage "ANNOTATOR" :chip "op-loop" :ibol "LOOP ARROW" :ibol-says ""
    :intents ["gist"] :r "—"
    :aif "The turn's main point, stated so it stands alone. It summarises the other notes rather than adding a move by Joe."}])

;; The feed's CSS styles .intent only inside a .note; the legend is not a
;; note, so it carries the same small caps and colour itself.
(def intent-style {:font-variant "small-caps" :letter-spacing ".04em" :color "#b8431f"})

(defn here-stats
  "Per intent on this page: notes, and how many cite a library pattern."
  [turns]
  (reduce (fn [m n]
            (-> m
                (update-in [(:intent n) :n] (fnil inc 0))
                (update-in [(:intent n) :with-pattern] (fnil + 0) (if (seq (:patterns n)) 1 0))))
          {}
          (for [t turns n (:notes t)] n)))

(defn fetch-mined! []
  ;; Written by futon3c/scripts/mined_intent_stats.py, which maps Kimi's open
  ;; labels onto this vocabulary through resources/turnfeed/intent-crosswalk.json.
  (-> (js/fetch (str "mined-intents.json?t=" (.now js/Date)) #js {:cache "no-store"})
      (.then #(if (.-ok %) (.json %) (throw (js/Error. (.-status %)))))
      (.then (fn [json] (swap! state assoc :mined (js->clj json))))
      (.catch #(swap! state assoc :mined {:error (str %)}))))

(defn pct [a b] (if (pos? b) (str (js/Math.round (* 100 (/ a b))) "%") "–"))

(def grey {:color "#888"})

(defn stat-cell
  "One intent's count, its share of the section, and the share of those
  notes that cite a pattern."
  [{:keys [n with-pattern]} total]
  (let [n (or n 0)]
    [:div {:style {:white-space "nowrap"}}
     n " · " (pct n total)
     [:span {:style grey} " · " (if (pos? n) (pct (or with-pattern 0) n) "–") " cite"]]))

(defn ibol-legend [turns mined]
  (let [here (here-stats turns)
        here-total (reduce + (map :n (vals here)))
        here-cited (reduce + (map :with-pattern (vals here)))
        mined-by (into {} (for [[k v] (get mined "by-intent")]
                            [k {:n (get v "n") :with-pattern (get v "with-pattern")}]))
        unmapped (get mined "unmapped")
        mined-total (+ (reduce + (map :n (vals mined-by))) (or (get unmapped "n") 0))
        charted (set (mapcat :intents legend-rows))
        unchipped (sort-by (comp - :n val) (remove (comp charted key) here))
        cell {:style {:padding ".35rem .6rem .35rem 0" :vertical-align "top"
                      :border-bottom "1px solid #eee"}}]
    [:details.ibol-legend {:style {:width "100%" :margin "0 0 2rem 0" :font-size ".78rem"
                                   :line-height 1.45 :color "#333"}}
     [:summary {:style {:cursor "pointer" :color "#b8431f"}}
      "Legend: chips, intents, and what a turn does to the agent's loop"]
     [:p {:style {:max-width "46rem"}}
      "Each note in the margin reads one span of Joe's turn as an "
      [:span.intent {:style intent-style} "intent"]
      ". The chip beside it is a ChipWits IBOL operator (Doug Sharp, 1984), "
      "kimi-2's alignment of that intent. The last two columns read the same span "
      "from the agent's side: an operator turn is an observation arriving at one "
      "stage of the agent's perceive–believe–evaluate–select–act loop, and the "
      "R-node names that stage in the War Machine catalogue. The R-node for each "
      "intent is a proposal (claude-12, 2026-09-25): it is right if turns read this "
      "way predict what the agent did next."]
     [:p {:style {:max-width "46rem"}}
      [:b "Here"] ": the notes on this page — " (count turns) " turns, " here-total
      " notes, " (pct here-cited here-total) " citing a library pattern. "
      [:b "Mined"] ": Kimi's reading of earlier turns (08-22 to 09-21) — "
      (if (get mined "turns")
        [:<> (get mined "turns") " turns, " mined-total " notes, "
         (pct (get mined "with-pattern") mined-total) " citing a pattern. Kimi's labels were "
         "open; a crosswalk maps the common ones onto this page's vocabulary. "
         (get unmapped "n") " notes (" (pct (get unmapped "n") mined-total) "), under "
         (get unmapped "labels") " labels that fit no one intent, are left out of the rows "
         "below but counted in the section's total."]
        (or (:error mined) "loading…"))
      " Each cell: notes · share of the section · share of those notes citing a pattern."]
     [:table {:style {:border-collapse "collapse" :width "100%"}}
      [:thead
       [:tr (for [h ["IBOL operator" "intent" "here" "mined" "R-node" "what the turn does, in AIF terms"]]
              ^{:key h} [:th (assoc-in cell [:style :text-align] "left") h])]]
      [:tbody
       (for [[stage gloss] loop-stages
             :let [rows (filter #(= stage (:stage %)) legend-rows)]
             :when (seq rows)]
         ^{:key stage}
         [:<>
          [:tr [:td {:col-span 6 :style {:padding ".9rem 0 .2rem 0" :font-variant "small-caps"
                                         :letter-spacing ".05em" :color "#555"}}
                (str (str/lower-case stage) " — " gloss)]]
          (for [{:keys [chip ibol ibol-says intents r aif]} rows]
            ^{:key (first intents)}
            [:tr
             ;; The notes' dropcap size, but the text keeps its own column:
             ;; a third line starts under the second, not under the chip.
             [:td cell
              [:div {:style {:display "flex" :align-items "flex-start"}}
               ;; One slot width for every chip, so the text lines up down
               ;; the column: the widest chips (15x16 at 2.7em) are ~2.55em.
               [:div {:style {:flex "none" :width "2.6em" :margin ".2em .5em 0 0"
                              :display "flex" :justify-content "center"}}
                (when chip
                  [:img.chip {:src (str "chips/" chip ".png") :alt "" :title chip
                              :style {:height "2.7em" :margin 0 :opacity 0.8}}])]
               [:div [:b ibol] [:br] [:span {:style {:color "#777"}} ibol-says]]]]
             [:td cell (for [i intents]
                         ^{:key i} [:div {:style {:white-space "nowrap"}}
                                    (when-let [m (get intent-mark i)] (str m " "))
                                    [:span.intent {:style intent-style} i]])]
             [:td cell (for [i intents] ^{:key i} [stat-cell (get here i) here-total])]
             [:td cell (for [i intents] ^{:key i} [stat-cell (get mined-by i) mined-total])]
             [:td cell r]
             [:td cell aif]])])]]
     (when (seq unchipped)
       [:p {:style {:color "#777"}}
        "Intents here with no chip: "
        (interpose ", " (for [[i {:keys [n]}] unchipped]
                          ^{:key i} [:<> [:span.intent {:style intent-style} i] " " n]))
        (when-let [m (get mined-by "provide-reference")]
          (str " (mined: provide-reference " (:n m) ")"))
        ". A missing icon is not worth a wrong one."])
     (when (seq (get unmapped "top"))
       [:p {:style {:color "#777"}}
        "Mined labels left unmapped, most frequent first: "
        (str/join ", " (for [[l n] (get unmapped "top")] (str l " " n)))
        ". They are evaluations whose polarity was not recorded, discourse markers, and "
        "labels whose examples split between two intents (futon3c "
        "resources/turnfeed/intent-crosswalk.json)."])]))

;; One feed covering every buffer, written by futon3c/scripts/turn_margin_html.py
;; --agent all. Capture is on in every agent buffer by default since
;; 2026-09-26, so a per-agent list here would silently drop the rest.
;; claude-12's turns still reach the records through
;; scripts/operator_turn_capture.py, since Joe types to it from another Emacs.
(def feed-urls ["feed-all.json"])
(def poll-ms 10000)

(defn- fetch-json [url]
  (-> (js/fetch (str url "?t=" (.now js/Date)) #js {:cache "no-store"})
      (.then #(if (.-ok %) (.json %) (throw (js/Error. (str url " " (.-status %))))))
      (.then #(js->clj % :keywordize-keys true))))

(defn fetch! []
  ;; A feed that fails to load is named in the error line and the others
  ;; still show; the turns are merged newest first.
  (-> (js/Promise.allSettled (clj->js (map fetch-json feed-urls)))
      (.then (fn [results]
               (let [rs (js->clj results :keywordize-keys true)
                     ok (mapcat :value (filter #(= "fulfilled" (:status %)) rs))
                     bad (keep #(when (= "rejected" (:status %)) (str (:reason %))) rs)]
                 (swap! state assoc
                        :turns (vec (sort-by :at #(compare %2 %1) ok))
                        :fetched-at (js/Date.)
                        :error (when (seq bad) (str/join "; " bad))))))))

;; --- rendering -------------------------------------------------------------

(defn run-span
  "One slice of the turn's text. A run with :n resolves to a note and is
  underlined; the rest is the surplus it sits in and stays unmarked. A run
  with :q is a block Joe quoted with >>> -- shown, never interpreted, and set
  in a fixed-width face because what he pastes is usually code."
  [{:keys [t n q]} lit]
  (cond
    q [:pre.quoted t]
    :else
  (if n
    [:span {:class (str "cue" (when (= n lit) " lit"))
            :on-click #(swap! state update :lit (fn [c] (when (not= c n) n)))}
     t]
    [:span t])))

(defn note-card [{:keys [id intent target rationale relations patterns]} lit]
  [:p {:class (str "note" (when (= id lit) " lit")) :id id
       :on-click #(swap! state assoc :lit id)}
   [chip intent] [:span.intent intent] " "
   (when (seq target) [:span.target target])
   (when (seq rationale) [:<> [:br] rationale])
   (when (seq relations) [:<> [:br] [:span.rel (str/join " · " relations)]])
   (for [p patterns]
     ^{:key (:id p)}
     [:<> [:br] [:span.pat (:id p)] " " (:why p)])])

(defn turn-block [{:keys [name at agent surface labeller runs notes sexp] :as turn} lit]
  (let [by (get turn (keyword "sexp-by"))]
    [:section.turn-block
     [:p.meta
      [:a {:href (str name ".html")} name] " · " at
      (when (seq agent) (str " · " agent))
      " · " surface " · "
      (if labeller (str "interpreted by " labeller) "not yet interpreted")]
     [:div.turn
      [:div.prose (for [[i run] (map-indexed vector runs)]
                    ^{:key i} [run-span run lit])]
      [:div.notes (if (seq notes)
                    (for [n notes] ^{:key (:id n)} [note-card n lit])
                    [:p.note.unresolved "Not yet interpreted."])]
      ;; Third column: the cascade. An authored one when a translator wrote
      ;; it, otherwise the annotation restated as an s-expression -- labelled
      ;; as derived, because nobody wrote it and it must not be read as a
      ;; translation.
      ;; Third column: the cascade, token by token. Each token carries what it
      ;; IS -- a resolving pattern id, a hole, an intent, plain markup -- so
      ;; the colour answers the question at a glance: which parts of this turn
      ;; actually reached the library? Tokens derived from a fragment carry its
      ;; note id and light with it, so clicking a cue in the prose shows the
      ;; sub-expression it produced.
      [:div.sexp
       (if (seq sexp)
         [:<>
          [:p.sexp-by (str "cascade · " by)]
          [:pre (for [[i {:keys [t k n]}] (map-indexed vector sexp)]
                  ^{:key i}
                  [:span {:class (str "tk-" k (when (and n (= n lit)) " lit"))
                          :on-click (when n
                                      #(swap! state update :lit
                                              (fn [c] (when (not= c n) n))))}
                   ;; No chips here: the cascade column is code, and an icon
                   ;; inside it breaks the alignment that makes an s-expression
                   ;; readable. The chips live in the notes.
                   t])]]
         [:p.note.unresolved "No cascade."])
       ;; Proposed flexiargs for the holes above. They are candidates, not
       ;; library entries: shown here so a name can be read and argued with
       ;; before anyone admits it.
       (when (seq (:candidates turn))
         [:div.candidates
          [:p.cand-head "proposed"]
          (for [c (:candidates turn)]
            ^{:key (:id c)}
            ;; A lineage, not a printed pattern: where the proposal hangs is
            ;; what a reader needs at a glance, and the body stays in the file.
            [:p.lineage {:title (:why c)}
             (if-let [p (:parent c)]
               [:<> [:span.known p] [:span.descends " ﹥ "]]
               [:span.toplevel "root ﹥ "])
             [:span.suggested (:id c)]
             [:span.cand-title " — " (:title c)]])])]]]))

(defn matches? [needle turn]
  (or (str/blank? needle)
      (str/includes? (str/lower-case (str/join " " (map :t (:runs turn))))
                     (str/lower-case needle))))

(defn app []
  (let [{:keys [turns fetched-at error lit filter mined]} @state
        shown (filterv #(matches? filter %) turns)]
    [:div
     [:h1 "Operator turns"]
     [:p.meta
      (count shown) " of " (count turns) " turns · "
      (if error (str "feed error: " error)
          (str "refreshed " (some-> fetched-at (.toLocaleTimeString))))
      " · polling every " (quot poll-ms 1000) "s"]
     [ibol-legend turns mined]
     [:p.legend
      [:span {:class "tk-pattern"} "pattern (resolves)"]
      [:span {:class "tk-dangling"} "id with no file"]
      [:span {:class "tk-hole"} "HOLE"]
      [:span {:class "tk-intent"} "intent only — markup, no pattern"]]
     [:input.filter {:placeholder "filter by text…"
                     :value filter
                     :on-change #(swap! state assoc :filter (-> % .-target .-value))}]
     (for [t shown] ^{:key (:name t)} [turn-block t lit])
     [:footer
      "Chip art by Doug Sharp, ChipWits 1984–86, CC BY-SA 4.0. "
      "Intent alignment proposed by kimi-2, 2026-09-23."]]))

(defonce root (delay (rdom-client/create-root (js/document.getElementById "app"))))

(defn ^:export init! []
  (rdom-client/render @root [app])
  (fetch!)
  (fetch-mined!)
  (js/setInterval fetch! poll-ms))
