#!/usr/bin/env bb
(ns snapshot-library-evidence
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [checks.library-graph-lint :as lint]))

(def attestations-path "library/aif/attestations.edn")
(def records-path "test/fixtures/library-graph/evidence-records.edn")
(def pin-path "test/fixtures/library-graph/evidence-records.pin.edn")

(defn write-edn! [path value]
  (spit path (with-out-str (pprint/pprint value))))

(let [attestations (edn/read-string (slurp attestations-path))
      ids (sort (set (map :id (mapcat :evidence attestations))))
      records (into (sorted-map)
                    (map (fn [id]
                           (let [record (lint/read-http-edn
                                         (str lint/evidence-store "/api/alpha/evidence/" id))]
                             (when-not record
                               (throw (ex-info "cited evidence record absent" {:id id})))
                             [id record])))
                    ids)
      serialized (with-out-str (pprint/pprint records))
      basis (lint/live-evidence-basis)]
  (spit records-path serialized)
  (write-edn! pin-path
              {:schema 1 :recorded-at (lint/now) :source lint/evidence-store
               :attestations attestations-path :record-count (count records)
               :content-sha256 (lint/sha256 serialized) :live-basis basis})
  (println (pr-str {:records (count records) :sha256 (lint/sha256 serialized)
                    :live-basis basis})))
