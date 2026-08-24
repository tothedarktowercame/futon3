(ns futon3.inbox-zero.confirm
  "Pure minting of a witnessed file claim from an exact-seat confirmation."
  (:require [clojure.string :as str]
            [futon3.inbox-zero.state :as state])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Date]))

(def ^:private confidences #{:direct :corroborated :weak})

(defn- non-blank-string? [value]
  (and (string? value) (not (str/blank? value))))

(defn- fail! [message error-type data]
  (throw (ex-info message (assoc data :error/type error-type))))

(defn- sha-256 [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str value) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- valid-time? [value]
  ;; Inbox-zero projections order timestamps as java.util.Date or epoch millis.
  ;; EDN #inst values read as Date, which is the normal durable representation.
  (or (instance? Date value) (integer? value)))

(defn- validate-proposal! [proposal]
  (let [path-key (:path/key proposal)
        candidate (:candidate proposal)
        evidence (:evidence candidate)]
    (when-not (and (map? proposal)
                   (map? path-key)
                   (every? #(non-blank-string? (get path-key %))
                           [:repo/id :worktree/id :path])
                   (map? candidate)
                   (non-blank-string? (:seat/id candidate))
                   (confidences (:confidence candidate))
                   (vector? evidence)
                   (seq evidence)
                   (every? #(and (map? %)
                                 (non-blank-string? (:source/id %)))
                           evidence))
      (fail! "Malformed attribution confirmation proposal"
             :inbox-zero/invalid-confirmation-proposal
             {:proposal proposal}))))

(defn- validate-confirmation! [confirmation]
  (when-not (map? confirmation)
    (fail! "Attribution confirmation must be a map"
           :inbox-zero/invalid-confirmation
           {:confirmation confirmation}))
  (when-not (non-blank-string? (:confirmed-by confirmation))
    (fail! "Attribution confirmation must name a seat"
           :inbox-zero/invalid-confirmation
           {:field :confirmed-by :value (:confirmed-by confirmation)}))
  (when-not (valid-time? (:at confirmation))
    (fail! "Attribution confirmation must contain an instant"
           :inbox-zero/invalid-confirmation
           {:field :at :value (:at confirmation)}))
  (when-not (non-blank-string? (:response/id confirmation))
    (fail! "Attribution confirmation must name its response"
           :inbox-zero/invalid-confirmation
           {:field :response/id :value (:response/id confirmation)})))

(defn confirmation-claim
  "Mint one deterministic claim from PROPOSAL and its exact-seat CONFIRMATION.

  The claim identity deliberately excludes response and confirmation time: a
  repeated confirmation of the same attribution evidence naturally dedupes at
  witness intake. This function performs no I/O."
  [proposal confirmation]
  (validate-proposal! proposal)
  (validate-confirmation! confirmation)
  (let [path-key (:path/key proposal)
        candidate (:candidate proposal)
        seat-id (:seat/id candidate)
        confirmer (:confirmed-by confirmation)]
    (when-not (= seat-id confirmer)
      (fail! "A seat may confirm only its own attribution"
             :inbox-zero/confirmer-mismatch
             {:candidate/seat-id seat-id :confirmed-by confirmer}))
    (let [evidence-ids (->> (:evidence candidate)
                            (map :source/id)
                            distinct
                            sort
                            vec)
          claim-id (str "claim:"
                        (sha-256 [seat-id (:worktree/id path-key)
                                  (:path path-key) evidence-ids]))
          claim {:record/type :inbox-zero/session-file-claim
                 :claim/id claim-id
                 :seat/id seat-id
                 :repo/id (:repo/id path-key)
                 :worktree/id (:worktree/id path-key)
                 :path (:path path-key)
                 :relation :edited-by-session
                 :witness/type :confirmed-attribution
                 :witness/id (:response/id confirmation)
                 :first-observed-at (:at confirmation)
                 :last-observed-at (:at confirmation)
                 :state :active
                 :attribution/evidence evidence-ids}]
      (state/validate-record claim))))
