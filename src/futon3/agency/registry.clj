(ns futon3.agency.registry
  "Unified agent registry for Agency.

   Manages local (in-JVM) agents with their invoke functions, session state,
   and lifecycle. Replaces the simpler local-handlers pattern with a richer
   registry that supports both Claude (persistent subprocess) and Codex
   (one-shot exec) invocation patterns.

   Design principles:
   - Single source of truth: All agent state in one registry
   - Concurrent safe: Clojure atoms for thread-safe updates
   - Type-aware: Different invoke patterns for Claude vs Codex
   - Session-tracked: Session-ids updated atomically with responses

   See: M-agency-unified-routing.md"
  (:import [java.time Instant]
           [java.util.concurrent TimeUnit]))

;; =============================================================================
;; Agent Registry
;; =============================================================================

(defonce ^{:doc "Registry of local agents.

   Structure: {agent-id -> agent-record}

   Agent record:
   {:agent-id      string
    :type          :claude | :codex
    :invoke-fn     (fn [prompt session-id] -> {:result :session-id :exit-code :error})
    :session-id    string (may be updated by invoke-fn)
    :registered-at Instant
    :last-active   Instant
    :subprocess    Process (for Claude persistent subprocess, nil for Codex)
    :metadata      map (arbitrary agent metadata)}"}
  agent-registry
  (atom {}))

;; =============================================================================
;; Registry Operations
;; =============================================================================

(defn registered-agents
  "Return list of registered agent IDs."
  []
  (keys @agent-registry))

(defn get-agent
  "Get agent record by ID, or nil if not registered."
  [agent-id]
  (get @agent-registry (name agent-id)))

(defn agent-registered?
  "Check if an agent is registered."
  [agent-id]
  (contains? @agent-registry (name agent-id)))

(defn register-agent!
  "Register an agent with the registry.

   Options:
     :agent-id   - Required. Unique identifier for the agent.
     :type       - Required. :claude or :codex
     :invoke-fn  - Required. Function (fn [prompt session-id] -> result-map)
     :session-id - Optional. Initial session ID to resume.
     :subprocess - Optional. Process handle for persistent subprocess (Claude).
     :cleanup-fn - Optional. Function to call on unregister (for subprocess cleanup).
     :metadata   - Optional. Arbitrary metadata map.

   If an agent with the same ID exists, it will be unregistered first (with cleanup).
   Returns the registered agent record."
  [{:keys [agent-id type invoke-fn session-id subprocess cleanup-fn metadata]}]
  {:pre [(some? agent-id)
         (#{:claude :codex} type)
         (fn? invoke-fn)]}
  (let [aid (name agent-id)
        now (Instant/now)]
    ;; Clean up existing agent if present (fix #5: guard against overwrite without cleanup)
    (when-let [existing (get @agent-registry aid)]
      (println (format "[registry] Replacing existing agent: %s" aid))
      (when-let [cleanup (:cleanup-fn existing)]
        (try (cleanup) (catch Exception e
                         (println (format "[registry] Cleanup error: %s" (.getMessage e))))))
      (when-let [proc (:subprocess existing)]
        (try
          (.destroy ^Process proc)
          (.waitFor ^Process proc 5 TimeUnit/SECONDS)
          (when (.isAlive ^Process proc)
            (.destroyForcibly ^Process proc))
          (catch Exception e
            (println (format "[registry] Subprocess cleanup error: %s" (.getMessage e)))))))
    (let [agent-record {:agent-id aid
                        :type type
                        :invoke-fn invoke-fn
                        :session-id session-id
                        :registered-at now
                        :last-active now
                        :subprocess subprocess
                        :cleanup-fn cleanup-fn
                        :metadata (or metadata {})}]
      (swap! agent-registry assoc aid agent-record)
      (println (format "[registry] Agent registered: %s (type=%s, session=%s)"
                       agent-id (name type) session-id))
      agent-record)))

(defn unregister-agent!
  "Unregister an agent and clean up resources.

   Calls cleanup-fn if provided, then stops subprocess with proper shutdown sequence.
   Returns true if agent was registered, false otherwise."
  [agent-id]
  (let [aid (name agent-id)]
    (if-let [agent (get @agent-registry aid)]
      (do
        ;; Call cleanup-fn first (fix #4: use richer cleanup)
        (when-let [cleanup (:cleanup-fn agent)]
          (println (format "[registry] Running cleanup for %s" aid))
          (try
            (cleanup)
            (catch Exception e
              (println (format "[registry] Cleanup error: %s" (.getMessage e))))))
        ;; Stop subprocess with proper shutdown sequence
        (when-let [proc (:subprocess agent)]
          (println (format "[registry] Stopping subprocess for %s" aid))
          (try
            (.destroy ^Process proc)
            (.waitFor ^Process proc 5 TimeUnit/SECONDS)
            (when (.isAlive ^Process proc)
              (println (format "[registry] Force-killing subprocess for %s" aid))
              (.destroyForcibly ^Process proc))
            (catch Exception e
              (println (format "[registry] Error stopping subprocess: %s" (.getMessage e))))))
        ;; Remove from registry
        (swap! agent-registry dissoc aid)
        (println (format "[registry] Agent unregistered: %s" aid))
        true)
      (do
        (println (format "[registry] Agent not found: %s" aid))
        false))))

(defn update-agent!
  "Update fields in an agent record.

   Usage: (update-agent! \"claude-1\" :session-id \"new-session\")"
  [agent-id & {:as updates}]
  (let [aid (name agent-id)]
    (when (contains? @agent-registry aid)
      (swap! agent-registry update aid merge updates)
      (get @agent-registry aid))))

(defn touch-agent!
  "Update last-active timestamp for an agent."
  [agent-id]
  (update-agent! agent-id :last-active (Instant/now)))

;; =============================================================================
;; Invocation
;; =============================================================================

(defn invoke-agent!
  "Invoke an agent with a prompt.

   Looks up the agent in the registry, calls its invoke-fn, updates
   session-id and last-active atomically.

   Options:
     timeout-ms - Optional timeout in milliseconds (default: no timeout)

   Returns:
     {:ok true :result ... :session-id ...} on success
     {:ok false :error ...} on failure"
  ([agent-id prompt] (invoke-agent! agent-id prompt nil))
  ([agent-id prompt timeout-ms]
   (let [aid (name agent-id)]
     (if-let [agent (get @agent-registry aid)]
       (let [invoke-fn (:invoke-fn agent)
             current-session (:session-id agent)]
         (try
           (println (format "[registry] Invoking %s (session=%s, prompt=%.50s...)"
                            aid current-session prompt))
           (let [;; Run invoke in a future if timeout specified (fix #1: timeout enforcement)
                 invoke-future (future (invoke-fn prompt current-session))
                 {:keys [result session-id exit-code error]}
                 (if timeout-ms
                   (deref invoke-future timeout-ms {:error "timeout" :exit-code -1})
                   @invoke-future)
                 effective-session (or session-id current-session)]
             ;; Fix #2: Only update if agent still exists with full record
             (swap! agent-registry
                    (fn [reg]
                      (if (and (contains? reg aid)
                               (:invoke-fn (get reg aid)))
                        (update reg aid assoc
                                :session-id effective-session
                                :last-active (Instant/now))
                        reg)))
             (if error
               {:ok false :error error :exit-code exit-code}
               ;; Fix #6: Return effective session-id, not just new one
               {:ok true :result result :session-id effective-session}))
           (catch Exception e
             (println (format "[registry] Invoke error for %s: %s" aid (.getMessage e)))
             {:ok false :error (.getMessage e)})))
       {:ok false :error (str "Agent not registered: " aid)}))))

;; =============================================================================
;; Introspection
;; =============================================================================

(defn registry-status
  "Return status of all registered agents."
  []
  {:agents
   (into {}
         (map (fn [[aid agent]]
                [aid {:type (:type agent)
                      :session-id (:session-id agent)
                      :registered-at (str (:registered-at agent))
                      :last-active (str (:last-active agent))
                      :has-subprocess (some? (:subprocess agent))}])
              @agent-registry))
   :count (count @agent-registry)})

(defn agent-types
  "Return map of agent counts by type."
  []
  (frequencies (map :type (vals @agent-registry))))

;; =============================================================================
;; Lifecycle Helpers
;; =============================================================================

(defn shutdown-all!
  "Unregister all agents and clean up resources.
   Call during graceful shutdown."
  []
  (println "[registry] Shutting down all agents...")
  (doseq [aid (keys @agent-registry)]
    (unregister-agent! aid))
  (println "[registry] All agents unregistered."))

(comment
  ;; REPL usage examples

  ;; Register a test agent
  (register-agent!
   {:agent-id "test-agent"
    :type :codex
    :invoke-fn (fn [prompt session-id]
                 {:result (str "Echo: " prompt)
                  :session-id session-id
                  :exit-code 0})})

  ;; Check status
  (registry-status)

  ;; Invoke
  (invoke-agent! "test-agent" "Hello!")

  ;; Unregister
  (unregister-agent! "test-agent")
  )
