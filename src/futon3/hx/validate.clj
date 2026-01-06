(ns futon3.hx.validate
  "Self-validation helpers for hypertext links (MVP)."
  (:require [f0.clock :as clock]
            [futon3.hx.link-types :as link-types]
            [futon3.hx.store :as store]))

(defn- anchor-exists? [state {:keys [artifact/id anchor/id]}]
  (if (and artifact/id anchor/id)
    (some #(= anchor/id (:anchor/id %)) (get-in state [:anchors artifact/id]))
    true))

(defn- artifact-exists? [state {:keys [artifact/id]}]
  (contains? (get-in state [:artifacts]) artifact/id))

(defn structural-validation [link]
  (let [state (store/state)
        from (:link/from link)
        to (:link/to link)
        issues (cond-> []
                 (not (artifact-exists? state from))
                 (conj {:issue :missing-from-artifact :artifact/id (:artifact/id from)})
                 (not (artifact-exists? state to))
                 (conj {:issue :missing-to-artifact :artifact/id (:artifact/id to)})
                 (not (anchor-exists? state from))
                 (conj {:issue :missing-from-anchor
                        :artifact/id (:artifact/id from)
                        :anchor/id (:anchor/id from)})
                 (not (anchor-exists? state to))
                 (conj {:issue :missing-to-anchor
                        :artifact/id (:artifact/id to)
                        :anchor/id (:anchor/id to)})
                 (not (link-types/allowed-type? (:link/type link)))
                 (conj {:issue :unknown-link-type :link/type (:link/type link)}))]
    {:ok? (empty? issues)
     :errors issues}))

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
