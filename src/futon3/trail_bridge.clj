(ns futon3.trail-bridge
  "Forward Tatami trail entries (intent → patterns + cues) to Futon1."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [f0.clock :as clock]
            [org.httpkit.client :as http])
  (:import (java.time Instant)
           (java.util UUID)))

(defn- parse-clock [value]
  (cond
    (nil? value) nil
    (instance? Number value) (long value)
    (string? value) (let [trimmed (str/trim value)]
                      (when (seq trimmed)
                        (try
                          (Long/parseLong trimmed)
                          (catch Exception _
                            (try
                              (.toEpochMilli (Instant/parse trimmed))
                              (catch Exception _ nil))))))
    :else nil))

(defn- iso->millis [value]
  (or (parse-clock value)
      (try
        (.toEpochMilli (Instant/parse value))
        (catch Exception _ nil))))

(defn- ensure-turn-id [entry]
  (or (:msg-id entry)
      (:turn-id entry)
      (:run-id entry)
      (str (UUID/randomUUID))))

(defn- primary-patterns [entry]
  (or (:patterns entry)
      (get-in entry [:tatami :patterns])
      []))

(defn- ensure-vec [value]
  (when (seq value)
    (vec value)))

(defn- trail-payload [entry cues embedding profile]
  (let [session-id (or (:session-id entry)
                       (get-in entry [:tatami :session-id]))
        clock-value (or (:clock entry)
                        (get-in entry [:tatami :clock]))
        ts (or (iso->millis clock-value)
               (System/currentTimeMillis))]
    (when session-id
      {:session-id session-id
       :turn-id (ensure-turn-id entry)
       :profile profile
       :timestamp ts
       :intent (:intent entry)
       :cue/text (or (:cue/text entry)
                     (:intent entry))
       :cue/pattern-id (or (:cue/pattern-id entry)
                           (some-> (:matches cues) first :pattern/id))
       :cue/timestamp (or (:cue/timestamp entry) ts)
       :patterns (vec (primary-patterns entry))
       :events (vec (:events entry))
       :futons (or (ensure-vec (:futons entry))
                   (ensure-vec (get-in entry [:tatami :futons])))
       :prototypes (or (ensure-vec (:prototypes entry))
                       (ensure-vec (get-in entry [:tatami :prototypes])))
       :fruits (:fruits cues)
       :paramitas (:paramitas cues)
       :matches (:matches cues)
       :embedding embedding
       :source "futon3"
       :raw entry})))

(defn publish!
  "Send the trail entry to Futon1 when enabled. CONFIG expects
   {:enabled? bool :api-base string :profile string :timeout-ms int}."
  [config entry cues embedding]
  (when (and (get config :enabled?)
             (seq (get config :api-base)))
    (when-let [payload (trail-payload entry cues embedding (or (:profile config) "default"))]
      (let [base (-> (:api-base config)
                     (str/replace #"/+$" ""))
            url (str base "/trails")
            headers (cond-> {"content-type" "application/json"}
                      (:profile config) (assoc "x-profile" (:profile config)))
            timeout (or (:timeout-ms config) 1500)]
        (http/post url {:headers headers
                        :timeout timeout
                        :body (json/encode payload)}
                   (fn [{:keys [error status]}]
                     (when error
                       (println "[trail-bridge] Futon1 POST failed:" (.getMessage error)))
                     (when (and status (>= status 400))
                       (println "[trail-bridge] Futon1 POST returned" status))))))))
