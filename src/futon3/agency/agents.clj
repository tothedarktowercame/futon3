(ns futon3.agency.agents
  "Convenience functions for registering agents with Agency.

   Combines the registry and invoke modules for easy agent setup.

   Usage:
     ;; Register a Codex agent
     (register-codex! \"codex-1\")

     ;; Register a Claude agent with session resume
     (register-claude! \"claude-1\" {:session-id \"abc123\"})

     ;; Check status
     (status)

     ;; Unregister
     (unregister! \"codex-1\")

   See: M-agency-unified-routing.md"
  (:require [futon3.agency.registry :as reg]
            [futon3.agency.invokes :as invokes]))

;; =============================================================================
;; Agent Registration Convenience Functions
;; =============================================================================

(defn register-codex!
  "Register a Codex agent with one-shot exec invocation.

   Options:
     :session-id - Thread ID to resume (optional)
     :metadata   - Arbitrary metadata (optional)"
  ([agent-id] (register-codex! agent-id {}))
  ([agent-id {:keys [session-id metadata]}]
   (reg/register-agent!
    {:agent-id agent-id
     :type :codex
     :invoke-fn (invokes/make-codex-invoke-fn)
     :session-id session-id
     :metadata (merge {:registered-via :register-codex!} metadata)})))

(defn register-claude!
  "Register a Claude agent with persistent subprocess.

   Options:
     :session-id - Session ID to resume (optional)
     :metadata   - Arbitrary metadata (optional)

   Returns the agent record, or nil if subprocess failed to start."
  ([agent-id] (register-claude! agent-id {}))
  ([agent-id {:keys [session-id metadata]}]
   (let [[invoke-fn subprocess] (invokes/make-claude-invoke-fn session-id)]
     (if invoke-fn
       (reg/register-agent!
        {:agent-id agent-id
         :type :claude
         :invoke-fn invoke-fn
         :session-id session-id
         :subprocess (:process subprocess)
         ;; Wire cleanup-fn to stop-claude-subprocess! (fix #4)
         :cleanup-fn (fn [] (invokes/stop-claude-subprocess! subprocess))
         :metadata (merge {:registered-via :register-claude!
                           :subprocess-state subprocess}
                          metadata)})
       (do
         (println (format "[agents] Failed to start Claude subprocess for %s" agent-id))
         nil)))))

(defn register-mock!
  "Register a mock agent for testing.

   Options:
     :response-fn - Function (fn [prompt session-id] -> response-string)
                    Default: echo back the prompt"
  ([agent-id] (register-mock! agent-id {}))
  ([agent-id {:keys [response-fn]}]
   (let [response-fn (or response-fn
                         (fn [prompt _session-id]
                           (str "Mock response to: " prompt)))]
     (reg/register-agent!
      {:agent-id agent-id
       :type :codex ; mock uses codex-style (no subprocess)
       :invoke-fn (fn [prompt session-id]
                    {:result (response-fn prompt session-id)
                     :session-id session-id
                     :exit-code 0})
       :metadata {:registered-via :register-mock!
                  :mock true}}))))

;; =============================================================================
;; Delegation to Registry
;; =============================================================================

(defn unregister!
  "Unregister an agent and clean up resources."
  [agent-id]
  (reg/unregister-agent! agent-id))

(defn status
  "Return status of all registered agents."
  []
  (reg/registry-status))

(defn invoke!
  "Invoke an agent with a prompt."
  [agent-id prompt]
  (reg/invoke-agent! agent-id prompt))

(defn registered?
  "Check if an agent is registered."
  [agent-id]
  (reg/agent-registered? agent-id))

(defn list-agents
  "Return list of registered agent IDs."
  []
  (reg/registered-agents))

(defn shutdown-all!
  "Unregister all agents and clean up resources."
  []
  (reg/shutdown-all!))

(comment
  ;; REPL usage examples

  ;; Register a mock agent
  (register-mock! "test-echo")
  (invoke! "test-echo" "Hello!")
  ;; => {:ok true, :result "Mock response to: Hello!", :session-id nil}

  ;; Register a Codex agent (requires codex CLI)
  (register-codex! "codex-1")
  (invoke! "codex-1" "Say hello in 5 words")

  ;; Register a Claude agent (requires claude CLI)
  (register-claude! "claude-1")
  (invoke! "claude-1" "Say hello!")

  ;; Check status
  (status)

  ;; List agents
  (list-agents)

  ;; Clean up
  (unregister! "test-echo")
  (shutdown-all!)
  )
