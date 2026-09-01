#!/usr/bin/env bb
;; build_step.bb (library-contract) -- ledger queries for library-build-loop.sh.
;; Same subcommands as wm-contract's; priority is ledger order (no run-lock rows here).
(require '[clojure.edn :as edn] '[clojure.string :as str])
(def path (str (.getParent (.getAbsoluteFile (java.io.File. *file*))) "/worklist.edn"))
(def w (edn/read-string (slurp path)))
(def items (:items w))
(def by-id (into {} (map (juxt :id identity) items)))
(defn loopable? [i]
  (and (= :open (:status i))
       (not= :J (:class i))
       (not= :joe (:owner i))
       (not (:loop-skip i))))
(def cmd (first *command-line-args*))
(case cmd
  ;; stall-key: id + status + progress content of the next-open row, so a
  ;; one-slice-per-invocation row that is COMMITTING slices does not read as
  ;; stalled (library loop false-stalled on L5 after 3 healthy slices,
  ;; 2026-09-01 20:34).
  "stall-key" (println (or (some->> (first (filter loopable? items))
                                    ((fn [i] (str (name (:id i)) ":" (name (:status i)) ":"
                                                  (hash (select-keys i [:progress :slice-b2a :slice-b2b]))))))
                           "NONE"))
  "next-open" (println (or (some-> (first (filter loopable? items)) :id name) "NONE"))
  "unreviewed" (println (str/join " " (map (comp name :id) (filter #(= :done-unreviewed (:status %)) items))))
  "registry-held" (println (if (some #(and (= :done-unreviewed (:status %))
                                           (not= :none (:covers-key %))) items) 1 0))
  "counts" (println (frequencies (map :status items)))
  "unblock"
  (let [ready (filter #(and (= :blocked (:status %)) (seq (:depends-on %))
                            (every? (fn [d] (= :done (:status (by-id d)))) (:depends-on %))
                            (not= :joe (:owner %)))
                      items)]
    (doseq [r ready]
      (let [s (slurp path)
            hdr (str "{:id " (:id r) " :class " (:class r) " :status :blocked")
            _ (when-not (str/includes? s hdr) (throw (ex-info "row header not found" {:id (:id r)})))
            s2 (str/replace-first s hdr (str "{:id " (:id r) " :class " (:class r) " :status :open :unblocked-by \"library-build-loop: depends-on all :done\""))]
        (spit path s2)
        (println "unblocked" (name (:id r))))))
  (do (println "usage: build_step.bb next-open|unblock|unreviewed|registry-held|counts") (System/exit 2)))
