#!/usr/bin/env bb
(ns checks.library-graph-lint
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def edge-kinds #{:why :how :see-also})
(def target-pattern #"[A-Za-z0-9_.-]+/[A-Za-z0-9_./'-]+")
(def evidence-export "/home/joe/code/futon1b/migration-export/evidence.edn")
(def evidence-cache
  (str (fs/path "/tmp" (str "futon3-spider-evidence-occurrences-"
                             (.toMillis (fs/last-modified-time evidence-export)) ".edn"))))

(defn sha256 [s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (format "%064x" (java.math.BigInteger. 1 (.digest digest (.getBytes s "UTF-8"))))))

(defn pattern-id [library file]
  (-> (str (fs/relativize library file))
      (str/replace #"\\" "/")
      (str/replace #"\.flexiarg$" "")))

(defn parse-pattern [library file]
  (let [text (slurp (str file))
        lines (str/split-lines text)
        from (pattern-id library file)
        body-start (or (first (keep-indexed
                               (fn [i line]
                                 (when (and (not (str/blank? line))
                                            (not (str/starts-with? line "@"))
                                            (not (str/starts-with? (str/triml line) ";;")))
                                   i))
                               lines))
                       (count lines))
        body (str/join "\n" (drop body-start lines))
        edges (mapcat
               (fn [[i line]]
                 (let [code (first (str/split line #";;" 2))]
                   (if-let [[_ kind tail] (re-matches #"\s*@(why|how|see-also)\s+(.+?)\s*" code)]
                     (for [token (str/split tail #"\s+")
                           :when (re-matches target-pattern token)]
                       {:from from :to token :kind (keyword kind)
                        :file (str (fs/relativize library file)) :line (inc i)})
                     [])))
               (map-indexed vector lines))]
    {:id from :file (str (fs/relativize library file))
     :body-line (when (< body-start (count lines)) (inc body-start))
     :body-digest (sha256 body) :edges (vec edges)}))

(defn scan-library [library]
  (let [files (sort (fs/glob library "**.flexiarg"))
        patterns (mapv #(parse-pattern library %) files)]
    {:patterns patterns
     :ids (set (map :id patterns))
     :edges (vec (mapcat :edges patterns))
     :body-digests (into (sorted-map) (map (juxt :file :body-digest) patterns))}))

(defn snapshot [scan]
  {:baseline/schema 1
   :edges (mapv #(select-keys % [:from :to :kind :file :line]) (:edges scan))
   :body-digests (:body-digests scan)})

(defn edge-key [edge] (select-keys edge [:from :to :kind]))

(defn cycle-failures [edges]
  (let [adj (reduce (fn [m {:keys [from kind] :as edge}]
                      (if (= kind :why) (update m from (fnil conj []) edge) m)) {} edges)
        colour (atom {})
        failures (atom [])]
    (letfn [(visit [node path via-edge]
              (case (get @colour node)
                :black nil
                :grey (swap! failures conj
                             {:check :why-acyclic
                              :file (:file via-edge) :line (:line via-edge)
                              :edge (edge-key via-edge)
                              :reason :why-cycle :cycle (conj (vec (drop-while #(not= % node) path)) node)})
                (do (swap! colour assoc node :grey)
                    (doseq [edge (get adj node)]
                      (visit (:to edge) (conj path node) edge))
                    (swap! colour assoc node :black))))]
      (doseq [node (keys adj)] (visit node [] nil))
      (vec (distinct @failures)))))

(defn nonblank-string? [x] (and (string? x) (not (str/blank? x))))

(defn valid-evidence? [x]
  (and (map? x)
       (every? #(contains? x %) [:id :via :query :excerpt])
       (some? (:id x)) (#{:tag :text} (:via x))
       (nonblank-string? (:query x)) (nonblank-string? (:excerpt x))))

(defn valid-state? [x]
  (or (#{:proposed :refused} x)
      (and (vector? x) (= 2 (count x)) (= :attested-by (first x))
           (nonblank-string? (second x)))))

(defn valid-attestation? [x]
  (and (map? x)
       (every? #(contains? x %) [:edge :by :at :read :cited :evidence :rung :state])
       (let [edge (:edge x)]
         (and (map? edge) (every? #(contains? edge %) [:from :to :kind])
              (nonblank-string? (:from edge)) (nonblank-string? (:to edge))
              (edge-kinds (:kind edge))))
       (nonblank-string? (:by x)) (nonblank-string? (:at x))
       (vector? (:read x)) (seq (:read x)) (every? nonblank-string? (:read x))
       (nonblank-string? (:cited x))
       (vector? (:evidence x)) (seq (:evidence x)) (every? valid-evidence? (:evidence x))
       (#{1 2} (:rung x)) (valid-state? (:state x))
       (or (not (contains? x :reason)) (nonblank-string? (:reason x)))
       (or (not= :refused (:state x)) (nonblank-string? (:reason x)))))

(defn string-leaves [x]
  (cond (map? x) (mapcat string-leaves (concat (keys x) (vals x)))
        (sequential? x) (mapcat string-leaves x)
        (string? x) [x]
        (keyword? x) [(subs (str x) 1)]
        :else []))

(defn normalized [x]
  (-> (str x) str/lower-case (str/replace #"\s+" " ") str/trim))

(defn evidence-record [records id]
  (if records
    (get records id)
    (try (edn/read-string (slurp (str "http://127.0.0.1:7073/api/alpha/evidence/" id)))
         (catch Exception _ nil))))

(defn rung-one-record [index pattern id]
  (some #(when (= id (:id %)) {:evidence/body (:excerpt %)}) (get index pattern)))

(defn attestation-semantic-failures [records export-index i att]
  (when (valid-attestation? att)
    (let [evidence (:evidence att)
          rung (:rung att)
          vias (set (map :via evidence))
          base {:check :attestation-semantics :line (inc i) :edge (:edge att)}]
      (concat
       (when (and (= rung 1) (not (contains? vias :tag)))
         [(assoc base :reason :rung-via-mismatch :detail :rung-1-requires-tag)])
       (when (and (= rung 2) (contains? vias :tag))
         [(assoc base :reason :rung-via-mismatch :detail :rung-2-forbids-tag)])
       (for [{:keys [id excerpt via]} evidence
             :let [record (if (= via :tag)
                            (if records (evidence-record records id)
                                (rung-one-record export-index (get-in att [:edge :from]) id))
                            (evidence-record records id))
                   haystack (normalized (str/join " " (string-leaves record)))]
             :when (or (nil? record) (not (str/includes? haystack (normalized excerpt))))]
         (assoc base :reason :evidence-excerpt-mismatch :evidence-id id))))))

(defn read-edn-or [path fallback]
  (if (and path (fs/exists? path)) (edn/read-string (slurp path)) fallback))

(defn section-summary [scan section]
  (let [prefix (str section "/")
        patterns (filter #(str/starts-with? (:id %) prefix) (:patterns scan))
        edges (filter #(str/starts-with? (:from %) prefix) (:edges scan))
        why-from (set (map :from (filter #(= :why (:kind %)) edges)))
        n (count patterns)]
    {:section section :patterns n
     :edges-by-kind (merge {:why 0 :how 0 :see-also 0} (frequencies (map :kind edges)))
     :patterns-with-outgoing-why (count why-from)
     :fraction-organised (if (zero? n) 0.0 (/ (double (count why-from)) n))}))

(defn lint [{:keys [library section baseline attestations evidence-records]}]
  (let [scan (scan-library library)
        base (read-edn-or baseline {:edges [] :body-digests {}})
        atts (read-edn-or attestations [])
        att-rows (if (vector? atts) atts [])
        records (when evidence-records (read-edn-or evidence-records {}))
        _ (when (and (nil? records)
                     (some #(some (fn [e] (= :tag (:via e))) (:evidence %)) att-rows)
                     (not (fs/exists? evidence-cache)))
            (let [repo (str (fs/parent (fs/absolutize library)))
                  result (process/shell {:continue true :out :string :err :string}
                                        "bb" "-cp" repo "-e"
                                        "(require 'checks.spider-runner) (force checks.spider-runner/evidence-index)")]
              (when-not (zero? (:exit result))
                (throw (ex-info "could not build exact-occurrence evidence index"
                                {:stderr (:err result)})))))
        export-index (when (and (nil? records) (fs/exists? evidence-cache))
                       (read-edn-or evidence-cache {}))
        baseline-edges (set (map edge-key (:edges base)))
        attested-edges (set (keep #(when (valid-attestation? %) (edge-key (:edge %))) att-rows))
        refused-edges (set (keep #(when (and (valid-attestation? %)
                                             (= :refused (:state %)))
                                    (edge-key (:edge %))) att-rows))
        prefix (str section "/")
        section-edges (filter #(str/starts-with? (:from %) prefix) (:edges scan))
        dangling (for [edge (:edges scan) :when (not (contains? (:ids scan) (:to edge)))]
                   (assoc (select-keys edge [:file :line :from :to :kind])
                          :check :targets-resolve :edge (edge-key edge) :reason :dangling-target))
        cycles (cycle-failures (:edges scan))
        missing-atts (for [edge section-edges
                           :let [k (edge-key edge)]
                           :when (and (not (contains? baseline-edges k))
                                      (not (contains? attested-edges k)))]
                       (assoc (select-keys edge [:file :line]) :check :new-edge-attested
                              :edge k :reason :new-edge-without-attestation))
        refused-present (for [edge section-edges
                              :let [k (edge-key edge)]
                              :when (contains? refused-edges k)]
                          (assoc (select-keys edge [:file :line]) :check :refused-edge-removed
                                 :edge k :reason :refused-edge-still-present))
        malformed (cond
                    (not (vector? atts))
                    [{:check :attestation-schema :file (str attestations)
                      :reason :malformed-attestation :detail :root-must-be-vector}]
                    :else
                    (keep-indexed (fn [i att]
                                    (when-not (valid-attestation? att)
                                      {:check :attestation-schema :file (str attestations)
                                       :line (inc i) :edge (:edge att)
                                       :reason :malformed-attestation}))
                                  att-rows))
        semantic (mapcat #(attestation-semantic-failures records export-index %1 %2)
                         (range) att-rows)
        body-failures (for [{:keys [file body-line body-digest]} (:patterns scan)
                            :when (str/starts-with? file prefix)
                            :let [old (get (:body-digests base) file)]
                            :when (not= old body-digest)]
                        {:check :argument-bodies-unchanged :file file :line body-line :edge nil
                         :reason (if old :argument-body-changed :argument-body-not-in-baseline)
                         :expected old :actual body-digest})
        failures (mapv #(merge {:file nil :line nil :edge nil} %)
                       (concat dangling cycles missing-atts refused-present malformed semantic body-failures))
        kind-counts (merge {:why 0 :how 0 :see-also 0}
                           (frequencies (map :kind (:edges scan))))
        why-nodes (set (mapcat (juxt :from :to) (filter #(= :why (:kind %)) (:edges scan))))]
    {:checks failures
     :summary (merge {:pass? (empty? failures)
                      :files (count (:patterns scan))
                      :edges-by-kind kind-counts
                      :patterns-in-why-graph (count why-nodes)
                      :unresolved-targets (count dangling)
                      :why-cycles (count cycles)
                      :failures (count failures)
                      :edge-counting :path-shaped-targets
                      :baseline-note (:measurement-note base)}
                     {:section (section-summary scan section)})}))

(defn parse-args [args]
  (when (odd? (count args)) (throw (ex-info "arguments must be --key value pairs" {})))
  (into {} (map (fn [[k v]] [(keyword (str/replace k #"^--" "")) v]) (partition 2 args))))

(defn -main [& args]
  (let [opts (parse-args args)
        report-path (:report opts)]
    (when-not (every? opts [:library :section :baseline :attestations :report])
      (binding [*out* *err*]
        (println "usage: library_graph_lint.clj --library DIR --section NAME --baseline FILE --attestations FILE --report FILE"))
      (System/exit 2))
    (let [report (try (lint opts)
                      (catch Exception e
                        {:checks [{:check :linter :file nil :line nil :edge nil
                                   :reason :linter-error :message (.getMessage e)}]
                         :summary {:pass? false :failures 1}}))]
      (fs/create-dirs (fs/parent report-path))
      (spit report-path (str (pr-str report) "\n"))
      (println (pr-str (:summary report)))
      (System/exit (if (get-in report [:summary :pass?]) 0 1)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
