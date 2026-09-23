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
   "redirect"   "arg-turn-right"  "collect"     "op-pickup"})

(defn chip [intent]
  (when-let [c (intent-chip intent)]
    [:img.chip {:src (str "chips/" c ".png") :alt "" :title (str intent " · " c)}]))

(def feed-url "feed-claude-1.json")
(def poll-ms 10000)

(defn fetch! []
  (-> (js/fetch (str feed-url "?t=" (.now js/Date)) #js {:cache "no-store"})
      (.then #(if (.-ok %) (.json %) (throw (js/Error. (.-status %)))))
      (.then (fn [json]
               (swap! state assoc
                      :turns (js->clj json :keywordize-keys true)
                      :fetched-at (js/Date.)
                      :error nil)))
      (.catch #(swap! state assoc :error (str %)))))

;; --- rendering -------------------------------------------------------------

(defn run-span
  "One slice of the turn's text. A run with :n resolves to a note and is
  underlined; the rest is the surplus it sits in and stays unmarked."
  [{:keys [t n]} lit]
  (if n
    [:span {:class (str "cue" (when (= n lit) " lit"))
            :on-click #(swap! state update :lit (fn [c] (when (not= c n) n)))}
     t]
    [:span t]))

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
                   (if (= k "intent") [:<> [chip t] t] t)])]]
         [:p.note.unresolved "No cascade."])
       ;; Proposed flexiargs for the holes above. They are candidates, not
       ;; library entries: shown here so a name can be read and argued with
       ;; before anyone admits it.
       (for [c (:candidates turn)]
         ^{:key (:id c)}
         [:div.candidate
          [:p.cand-id "? " (:id c) " — " (:title c)]
          [:dl
           (for [[label k] [["context" :context] ["IF" :if] ["HOWEVER" :however]
                            ["THEN" :then] ["BECAUSE" :because]
                            ["tried first" :tried]]
                 :let [v (get c k)] :when (seq v)]
             ^{:key label} [:<> [:dt label] [:dd v]])]])]]]))

(defn matches? [needle turn]
  (or (str/blank? needle)
      (str/includes? (str/lower-case (str/join " " (map :t (:runs turn))))
                     (str/lower-case needle))))

(defn app []
  (let [{:keys [turns fetched-at error lit filter]} @state
        shown (filterv #(matches? filter %) turns)]
    [:div
     [:h1 "Operator turns"]
     [:p.meta
      (count shown) " of " (count turns) " turns · "
      (if error (str "feed error: " error)
          (str "refreshed " (some-> fetched-at (.toLocaleTimeString))))
      " · polling every " (quot poll-ms 1000) "s"]
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
  (js/setInterval fetch! poll-ms))
