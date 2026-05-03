#!/usr/bin/env bb
;; Diachronic driver for v0_codebase_hypergraph.clj.
;;
;; For each commit in the target repo's history (or a sampled subset),
;; checks the commit into a worktree, runs the v0 projection (with
;; optional --vocab paths), captures the per-rev report, and aggregates
;; counts + drift signals into a single time-series EDN.
;;
;; Usage:
;;   bb diachronic_v0.clj <target-repo> [--every N] [--max M]
;;       [--vocab path]... [--out file.edn]
;;
;; Notes:
;; - Uses `git worktree add /tmp/diachronic-<sha>` so the live tree is
;;   untouched.
;; - Cleans up worktrees after each rev (`git worktree remove`).
;; - Runs the v0 script via subprocess; the v0 script must be in the
;;   same directory as this driver.

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.java.shell :refer [sh]]
         '[babashka.fs :as fs])

(defn run [& args]
  (let [{:keys [exit out err]} (apply sh args)]
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "[ERR]" args)
        (println err)))
    (str/trim (or out ""))))

(defn list-commits [repo every max-n]
  (let [all (str/split-lines
             (run "git" "-C" repo "log" "--oneline" "--reverse"))
        picked (->> all
                    (map-indexed vector)
                    (filter (fn [[i _]] (zero? (mod i every))))
                    (map second)
                    (take max-n))]
    (mapv (fn [ln]
            (let [[sha & rest] (str/split ln #"\s+" 2)]
              {:sha sha :subject (or (first rest) "")}))
          picked)))

(defn worktree-add [repo sha dir]
  (run "git" "-C" repo "worktree" "add" "--detach" dir sha))

(defn worktree-remove [repo dir]
  (run "git" "-C" repo "worktree" "remove" "--force" dir))

(defn run-v0 [v0-script wt vocab-args]
  (let [out-edn (str wt "-report.edn")
        args (concat ["bb" v0-script wt
                      "--out" out-edn]
                     vocab-args)]
    (apply run args)
    (when (fs/exists? out-edn)
      (let [r (edn/read-string (slurp out-edn))]
        (fs/delete out-edn)
        r))))

(defn distil-rev
  "Pull the time-series-relevant numbers out of a v0 report."
  [{:keys [counts components-summary drift-hotspots] :as r}]
  (let [comp0 (first (filter #(= 0 (:component %)) components-summary))
        comp2 (first (filter #(= 2 (:component %)) components-summary))
        comp3 (first (filter #(= 3 (:component %)) components-summary))
        big-comps (->> components-summary
                       (sort-by (comp - :size))
                       (take 5)
                       (mapv (fn [c]
                               {:size (:size c)
                                :n-terms (:n-terms c)
                                :sample-ns (vec (take 3 (:sample-namespaces c)))})))]
    {:vars (:vars counts)
     :tests (:tests counts)
     :coverage-edges (:coverage-edges counts)
     :call-edges (:call-edges counts)
     :vocab-edges (:vocab-edges counts)
     :term-count (:term-count counts)
     :paired-vars (:paired-vars counts)
     :coverage-pct (:coverage-pct counts)
     :components (:components counts)
     :namespaces (:namespaces counts)
     :big-components big-comps
     :drift-hotspot-count (count (or drift-hotspots []))}))

(defn parse-args [argv]
  (loop [a argv opts {:every 1 :max 1000 :vocab []}]
    (cond
      (empty? a) opts
      (= "--every" (first a)) (recur (drop 2 a) (assoc opts :every (parse-long (second a))))
      (= "--max"   (first a)) (recur (drop 2 a) (assoc opts :max   (parse-long (second a))))
      (= "--vocab" (first a)) (recur (drop 2 a) (update opts :vocab #(conj % "--vocab" (second a))))
      (= "--out"   (first a)) (recur (drop 2 a) (assoc opts :out (second a)))
      (str/starts-with? (first a) "--") (recur (rest a) opts)
      :else (recur (rest a) (assoc opts :repo (first a))))))

(defn -main [& argv]
  (let [opts (parse-args argv)
        repo (or (:repo opts) (System/getProperty "user.dir"))
        v0   (str (fs/parent (fs/absolutize *file*)) "/v0_codebase_hypergraph.clj")
        commits (list-commits repo (:every opts) (:max opts))
        wt-base "/tmp/diachronic"]
    (println "Repo:" repo)
    (println "v0 script:" v0)
    (println "Commits to process:" (count commits))
    (println)
    (let [series
          (vec
           (for [[idx {:keys [sha subject]}] (map-indexed vector commits)]
             (let [wt (str wt-base "-" sha)]
               (println (format "[%d/%d] %s  %s" (inc idx) (count commits) sha
                                (subs subject 0 (min 60 (count subject)))))
               (worktree-add repo sha wt)
               (let [r (try (run-v0 v0 wt (:vocab opts))
                            (catch Exception e (println "  err:" (.getMessage e)) nil))]
                 (worktree-remove repo wt)
                 (merge {:sha sha :subject subject :idx idx}
                        (when r (distil-rev r)))))))]
      (let [out (or (:out opts) "/tmp/diachronic-series.edn")]
        (spit out (with-out-str (clojure.pprint/pprint
                                  {:repo repo
                                   :v0-script v0
                                   :every (:every opts)
                                   :n-commits (count commits)
                                   :series series})))
        (println)
        (println "Wrote" out))
      ;; quick summary table
      (println)
      (println "idx  sha       vars  tests  cov  call  vocab  comp  paired  cov%   drift")
      (doseq [r series]
        (println (format "%-4d %-9s %4s  %4s  %3s  %4s  %5s  %4s  %5s  %5.1f  %3s"
                         (:idx r)
                         (subs (or (:sha r) "") 0 (min 8 (count (or (:sha r) ""))))
                         (or (:vars r) "-") (or (:tests r) "-")
                         (or (:coverage-edges r) "-") (or (:call-edges r) "-")
                         (or (:vocab-edges r) "-") (or (:components r) "-")
                         (or (:paired-vars r) "-")
                         (double (or (:coverage-pct r) 0))
                         (or (:drift-hotspot-count r) "-")))))))

(apply -main *command-line-args*)
