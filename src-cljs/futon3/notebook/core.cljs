(ns futon3.notebook.core
  "Live notebook viewer - streams conversation turns via SSE"
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom-client]
            [clojure.string :as str]))

;; State
(defonce app-state
  (r/atom {:session-id nil
           :events []
           :connected? false
           :error nil}))

;; SSE Connection
(defonce event-source (atom nil))

(defn parse-event [data]
  (try
    (js->clj (js/JSON.parse data) :keywordize-keys true)
    (catch :default e
      (js/console.error "Failed to parse event:" e)
      nil)))

(defn connect-sse! [session-id]
  (when @event-source
    (.close @event-source))

  (let [url (str "/fulab/notebook/" session-id "/stream")
        source (js/EventSource. url)]

    (set! (.-onopen source)
          (fn [_]
            (js/console.log "SSE connected")
            (swap! app-state assoc :connected? true :error nil)))

    (set! (.-onmessage source)
          (fn [e]
            (when-let [event (parse-event (.-data e))]
              (case (:type event)
                "init" (swap! app-state assoc
                              :session-id (:session-id event)
                              :events (vec (:events event)))
                "turn" (swap! app-state update :events conj (:event event))
                (js/console.log "Unknown event type:" (:type event))))))

    (set! (.-onerror source)
          (fn [e]
            (js/console.error "SSE error:" e)
            (swap! app-state assoc :connected? false :error "Connection lost")))

    (reset! event-source source)))

(defn disconnect! []
  (when @event-source
    (.close @event-source)
    (reset! event-source nil)
    (swap! app-state assoc :connected? false)))

;; Components
(defn format-timestamp [ts]
  (when ts
    (let [d (js/Date. ts)]
      (.toLocaleTimeString d))))

(defn normalize-role [role]
  (cond
    (keyword? role) role
    (string? role) (keyword role)
    :else :unknown))

(defn turn-component [{:keys [event/type payload at]}]
  (let [role (normalize-role (get payload :role))
        content (get payload :content)
        role-class (case role
                     :user "user"
                     :agent "agent"
                     "system")]
    [:div.turn {:class role-class}
     [:div.turn-header
      [:span.role (str/upper-case (name (or role type "unknown")))]
      [:span.timestamp (format-timestamp at)]]
     [:div.turn-content content]]))

(defn system-event-component [{:keys [event/type at payload]}]
  [:div.turn.system
   [:div.turn-header
    [:span.role (if (keyword? type) (name type) (str type))]
    [:span.timestamp (format-timestamp at)]]
   [:div.turn-content
    (pr-str payload)]])

(defn event-component [event]
  (let [etype (:event/type event)
        ;; Handle both keyword and string event types from JSON
        etype-str (if (keyword? etype) (name etype) (str etype))]
    (if (#{"user" "agent" "turn/user" "turn/agent"} etype-str)
      [turn-component event]
      [system-event-component event])))

(defn connection-status []
  (let [{:keys [connected? error]} @app-state]
    [:div.connection-status {:class (if connected? "connected" "disconnected")}
     (if connected?
       [:span "● Live"]
       [:span (or error "Connecting...")])]))

(defn notebook-header []
  (let [{:keys [session-id events]} @app-state
        turns (filter #(#{:turn/user :turn/agent} (:event/type %)) events)]
    [:header
     [:h1 (or session-id "Scribe Notebook")]
     [:div.meta
      [:span (str "Events: " (count events))]
      [:span (str "Turns: " (count turns))]
      [connection-status]]]))

(defn notebook-body []
  (let [{:keys [events]} @app-state]
    [:main
     (for [[idx event] (map-indexed vector events)]
       ^{:key idx}
       [event-component event])]))

(defn summary-stats []
  (let [{:keys [events]} @app-state
        user-turns (filter #(= :turn/user (:event/type %)) events)
        agent-turns (filter #(= :turn/agent (:event/type %)) events)]
    [:div.summary
     [:h2 "Session Summary"]
     [:div.summary-grid
      [:div.stat
       [:div.stat-value (count user-turns)]
       [:div.stat-label "User Turns"]]
      [:div.stat
       [:div.stat-value (count agent-turns)]
       [:div.stat-label "Agent Turns"]]
      [:div.stat
       [:div.stat-value (count events)]
       [:div.stat-label "Total Events"]]]]))

(defn app []
  [:div.container
   [notebook-header]
   [notebook-body]
   [summary-stats]
   [:footer "Scribe Notebook · Live Stream"]])

;; Initialization
(defonce root-instance (atom nil))

(defn get-session-id-from-url []
  (let [path (.-pathname js/location)
        match (re-find #"/notebook/([^/]+)" path)]
    (second match)))

(defn init! []
  (js/console.log "Initializing notebook viewer")
  (when-let [container (.getElementById js/document "app")]
    (let [root (rdom-client/create-root container)]
      (reset! root-instance root)
      (rdom-client/render root [app]))
    (when-let [session-id (get-session-id-from-url)]
      (js/console.log "Connecting to session:" session-id)
      (connect-sse! session-id))))

;; Hot reload support
(defn ^:dev/after-load reload! []
  (js/console.log "Hot reload")
  (when-let [root @root-instance]
    (rdom-client/render root [app])))
