#!/usr/bin/env bb
;; Operator-facing cross-root move harness for ~/code/futonX and ~/code/futonY.
;;
;; Modes:
;;   --mode single     one unique Clojure file moved X -> Y
;;   --mode batch      N unique Clojure files moved X -> Y
;;   --mode ambiguous  N identical Python files moved X -> Y to probe the
;;                     conservative same-hash pairing rule
;;
;; Reports watcher observations plus per-file substrate summaries.

(require '[clojure.java.shell :refer [sh]]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http]
         '[clojure.edn :as edn]
         '[clojure.pprint :as pp])

(load-file "/home/joe/code/futon3/scripts/multi_watcher.clj")

(def WATCHER-SCRIPT "/home/joe/code/futon3/scripts/multi_watcher.clj")
(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))

(defn parse-args [argv]
  (loop [a argv
         opts {:root-x "/home/joe/code/futonX"
               :root-y "/home/joe/code/futonY"
               :label-x "futonX"
               :label-y "futonY"
               :mode "single"
               :count 3
               :interval-ms 500
               :max-cycles 12
               :keep? false}]
    (cond
      (empty? a) opts
      (= "--root-x" (first a)) (recur (drop 2 a) (assoc opts :root-x (second a)))
      (= "--root-y" (first a)) (recur (drop 2 a) (assoc opts :root-y (second a)))
      (= "--label-x" (first a)) (recur (drop 2 a) (assoc opts :label-x (second a)))
      (= "--label-y" (first a)) (recur (drop 2 a) (assoc opts :label-y (second a)))
      (= "--mode" (first a)) (recur (drop 2 a) (assoc opts :mode (second a)))
      (= "--count" (first a)) (recur (drop 2 a) (assoc opts :count (parse-long (second a))))
      (= "--interval-ms" (first a)) (recur (drop 2 a) (assoc opts :interval-ms (parse-long (second a))))
      (= "--max-cycles" (first a)) (recur (drop 2 a) (assoc opts :max-cycles (parse-long (second a))))
      (= "--keep" (first a)) (recur (rest a) (assoc opts :keep? true))
      :else (throw (ex-info "unknown-arg" {:arg (first a)})))))

(defn prop-get [h k]
  (or (get-in h [:hx/props k])
      (get-in h [:hx/props (name k)])))

(defn http-get-edn [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER}
                            :throw false})]
    (when (= 200 (:status resp))
      (edn/read-string (:body resp)))))

(defn fetch-by-end [eid]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?end="
                             (java.net.URLEncoder/encode eid "UTF-8")))]
    (or (:hyperedges r) [])))

(defn type-str [x]
  (cond
    (keyword? x) (if (namespace x)
                   (str (namespace x) "/" (name x))
                   (name x))
    (string? x) x
    :else (str x)))

(defn summarize-var [h]
  (when h
    {:endpoint (first (:hx/endpoints h))
     :source-file (prop-get h :source-file)
     :stale (true? (prop-get h :edge/witness-stale))
     :repo (prop-get h :repo)}))

(defn clj-case [stamp idx root-x root-y]
  (let [suffix (format "%02d" idx)
        ns-name (str "watcher.cross.harness.t" stamp "." suffix)
        var-name (str "hopped-" suffix)
        qname (str ns-name "/" var-name)
        file-name (str "cross_root_probe_" stamp "_" suffix ".clj")]
    {:kind :clj
     :id suffix
     :qname qname
     :old-file (str root-x "/" file-name)
     :new-file (str root-y "/" file-name)
     :content (str "(ns " ns-name ")\n"
                   "(defn " var-name " [] :ok)\n")}))

(def identical-python-body
  "VALUE = 1\n")

(defn ambiguous-python-case [stamp idx root-x root-y]
  (let [suffix (format "%02d" idx)
        file-name (str "ambiguous_cross_root_" stamp "_" suffix ".py")]
    {:kind :py
     :id suffix
     :old-file (str root-x "/" file-name)
     :new-file (str root-y "/" file-name)
     :content identical-python-body}))

(defn build-cases [mode count root-x root-y stamp]
  (case mode
    "single" [(clj-case stamp 1 root-x root-y)]
    "batch" (mapv #(clj-case stamp % root-x root-y) (range 1 (inc count)))
    "ambiguous" (mapv #(ambiguous-python-case stamp % root-x root-y) (range 1 (inc count)))
    (throw (ex-info "unknown-mode" {:mode mode}))))

(defn summarize-case [label-x label-y {:keys [id kind old-file new-file qname]}]
  (binding [*out* *err*]
    (println "[test-futons] summarizing case" id kind))
  (if (nil? qname)
    {:id id
     :kind kind
     :old-file old-file
     :new-file new-file
     :summary "endpoint summary skipped for ambiguous mode; use watcher counts"}
    (let [old-end (str label-x "/" qname)
          new-end (str label-y "/" qname)
          old-var (->> (fetch-by-end old-end)
                       (filter #(= "code/v05/var" (type-str (:hx/type %))))
                       first)
          new-var (->> (fetch-by-end new-end)
                       (filter #(= "code/v05/var" (type-str (:hx/type %))))
                       first)
          rename-link? (->> (fetch-by-end old-end)
                            (filter #(= "edge/renamed-to" (type-str (:hx/type %))))
                            (some #(some #{new-end} (:hx/endpoints %))))]
      {:id id
       :kind kind
       :old-file old-file
       :new-file new-file
       :old (summarize-var old-var)
       :new (summarize-var new-var)
       :renamed-link-present? (boolean rename-link?)})))

(defn observed-count [needle s]
  (count (re-seq (re-pattern (java.util.regex.Pattern/quote needle)) (or s ""))))

(defn cleanup-cases! [cases]
  (doseq [{:keys [old-file new-file]} cases]
    (when (fs/exists? old-file) (fs/delete old-file))
    (when (fs/exists? new-file) (fs/delete new-file))))

(defn -main [& argv]
  (let [{:keys [root-x root-y label-x label-y mode interval-ms max-cycles keep?]
         case-count :count} (parse-args argv)
        stamp (System/currentTimeMillis)
        cases (build-cases mode case-count root-x root-y stamp)
        report-path (str "/tmp/test-futons-cross-root-" stamp ".edn")]
    (binding [*out* *err*]
      (println "[test-futons] mode" mode "count" (count cases) "roots" root-x root-y))
    (fs/create-dirs root-x)
    (fs/create-dirs root-y)
    (doseq [{:keys [old-file content]} cases]
      (spit old-file content))
    (let [watcher (future
                    (sh "bb" WATCHER-SCRIPT
                        "--root" (str root-x "=" label-x)
                        "--root" (str root-y "=" label-y)
                        "--interval-ms" (str interval-ms)
                        "--max-cycles" (str max-cycles)))]
      (try
        (binding [*out* *err*]
          (println "[test-futons] waiting for cold scan"))
        (Thread/sleep 1800)
        (doseq [{:keys [old-file new-file]} cases]
          (fs/move old-file new-file))
        (binding [*out* *err*]
          (println "[test-futons] move complete; waiting for watcher"))
        (let [{:keys [exit err out]} @watcher
              _ (binding [*out* *err*]
                  (println "[test-futons] watcher exit" exit))
              report {:mode mode
                      :count (count cases)
                      :roots {:x root-x :y root-y}
                      :labels {:x label-x :y label-y}
                      :files (mapv #(select-keys % [:id :kind :old-file :new-file]) cases)
                      :watcher {:exit exit
                                :stderr err
                                :cross-root-move-count
                                (reduce + (map #(observed-count (str "[cross-root-move] " (:old-file %) " → " (:new-file %))
                                                                out)
                                               cases))
                                :cross-root-cascade-count
                                (reduce + (map #(observed-count (str "[cross-root-cascade] " (:old-file %) " → " (:new-file %))
                                                                out)
                                               cases))}
                      :cases (mapv #(summarize-case label-x label-y %) cases)}
              _ (when-not keep?
                  (cleanup-cases! cases))]
          (spit report-path (with-out-str (pp/pprint report)))
          (println "[test-futons] report written to" report-path)
          (pp/pprint report)
          (shutdown-agents)
          (System/exit exit))
        (finally
          @watcher
          (when-not keep?
            (cleanup-cases! cases)))))))

(apply -main *command-line-args*)
