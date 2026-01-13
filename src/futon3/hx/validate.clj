(ns futon3.hx.validate
  "Self-validation helpers for hypertext links (MVP)."
  (:require [f0.clock :as clock]
            [futon3.logic-audit :as logic-audit]
            [futon3.hx.logic :as logic]))

(def ^:dynamic *audit-context* nil)

(defn structural-step
  "Run structural checks for a canonical hx step and return validation map."
  [step]
  (let [result (logic/check-step step)
        context (when (map? *audit-context*) *audit-context*)]
    (logic-audit/record! (merge {:scope :hx
                                 :op :hx/structural-step
                                 :result result}
                                context))
    {:ok? (:ok? result)
     :errors (:errors result)
     :logic (:logic result)}))

(defn structural-validation [link]
  (structural-step {:hx.step/kind :hx/link-suggest
                    :hx.step/payload {:link link}}))

(defn self-validation
  "Build a validation payload using agent-supplied status.
   STATUS should be :accepted or :rejected; defaults to :needs-review."
  ([link agent] (self-validation link agent :needs-review nil))
  ([link agent status] (self-validation link agent status nil))
  ([link agent status rationale]
   (let [structural (structural-validation link)]
     (cond-> {:validation/status (or status :needs-review)
              :validation/structural structural
              :validation/agent agent
              :validation/timestamp (clock/->iso-string)}
       rationale (assoc :validation/semantic {:ok? (= status :accepted)
                                              :model "self"
                                              :confidence 0.5
                                              :rationale rationale})))))

(defn peer-review
  "Attach a peer sign-off (e.g., fucodex -> fubar) to a validation payload."
  [validation peer-agent status rationale]
  (assoc validation :validation/peer {:agent peer-agent
                                      :status (or status :needs-review)
                                      :rationale rationale
                                      :timestamp (clock/->iso-string)}))
