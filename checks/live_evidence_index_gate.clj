#!/usr/bin/env bb
(ns checks.live-evidence-index-gate
  (:require [checks.library-graph-lint :as lint]))

(defn valid-basis? [basis]
  (and (nat-int? (:count basis))
       (pos? (:count basis))
       (string? (:max-at basis))
       (not (clojure.string/blank? (:max-at basis)))))

(defn main [args]
  (let [negative? (some #{"--negative"} args)
        basis (if negative? {:count -1 :max-at nil} (lint/live-evidence-basis))]
    (println (pr-str {:store lint/evidence-store :basis basis :bounded-requests 2}))
    (if negative?
      (if (valid-basis? basis)
        (do (println "live-evidence-index-gate: FAIL mutation slipped") 2)
        (do (println "live-evidence-index-gate: PASS malformed-basis mutation rejected") 0))
      (if (valid-basis? basis)
        (do (println "live-evidence-index-gate: PASS") 0)
        (do (println "live-evidence-index-gate: FAIL") 1)))))

(System/exit (main *command-line-args*))
