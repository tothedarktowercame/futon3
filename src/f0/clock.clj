(ns f0.clock
  "Minimal clock + timestamp helpers used for logging ingress/egress events."
  (:import (java.time Clock Instant OffsetDateTime ZoneOffset)
           (java.time.format DateTimeFormatter)))

(def formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSX"))

(defn system-clock [] (Clock/systemUTC))

(defn now
  ([] (now (system-clock)))
  ([^Clock clock] (Instant/now clock)))

(defn ->iso-string
  ([] (->iso-string (now)))
  ([^Instant instant]
   (.format formatter (OffsetDateTime/ofInstant instant ZoneOffset/UTC))))

(defn monotonic-millis []
  (-> (System/nanoTime) (/ 1e6) long))

(defn stamp
  "Return a log stamp {:at ts :seq monotonic} for quick auditing."
  ([] (stamp (now)))
  ([instant]
   {:at (->iso-string instant)
    :seq (monotonic-millis)}))
