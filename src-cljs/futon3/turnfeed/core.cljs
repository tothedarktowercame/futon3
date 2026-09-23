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
   [:span.intent intent] " "
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
      [:div.sexp
       (if sexp
         [:<> [:p.sexp-by (str "cascade · " by)] [:pre sexp]]
         [:p.note.unresolved "No cascade."])]]]))

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
     [:input.filter {:placeholder "filter by text…"
                     :value filter
                     :on-change #(swap! state assoc :filter (-> % .-target .-value))}]
     (for [t shown] ^{:key (:name t)} [turn-block t lit])]))

(defonce root (delay (rdom-client/create-root (js/document.getElementById "app"))))

(defn ^:export init! []
  (rdom-client/render @root [app])
  (fetch!)
  (js/setInterval fetch! poll-ms))
