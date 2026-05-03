#!/usr/bin/env bb
;; substrate-2 phase-5: futonic-zapper signature emission.
;;
;; Usage:
;;   bb phase_5_signatures.clj [--label <repo-label>]... [--all-labels]
;;       [--theta 0.05] [--out file.edn]
;;
;; For each active label, runs four signature queries (v0):
;;   (1) adapter shim that doesn't adapt
;;   (2) work-around (drift-without-structure)
;;   (3) concept used without definition
;;   (4) completion rot (substrate-2 specialisation)
;;
;; Each detected signature is emitted as a code/v05/satisficing-signature
;; hyperedge. Idempotent on stable IDs.
;;
;; v0 signatures are cross-sectional (no commit time-series); v0.5 will add
;; the four time-series signatures (quick-fix, coverage-retreat,
;; load-bearing-under-tested, concept-introduced-without-attachment).

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.set]
         '[babashka.http-client :as http]
         '[cheshire.core :as json])

(def FUTON1A   (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))

;; ---------- HTTP write ----------

(defn directed-endpoints [hx-type endpoints]
  (if (and (= "code/v05/satisficing-signature" hx-type) (= 2 (count endpoints)))
    (conj (vec endpoints) (str "dir:" (first endpoints) "→" (second endpoints)))
    endpoints))

(defn post-hyperedge!
  [hx-type endpoints labels & [props]]
  (let [endpoints (directed-endpoints hx-type endpoints)
        payload (cond-> {"hx/type" hx-type "hx/endpoints" endpoints}
                  (seq labels) (assoc "hx/labels" labels)
                  props (assoc "hx/props" props))]
    (try
      (let [resp (http/post (str FUTON1A "/api/alpha/hyperedge")
                            {:headers {"Content-Type" "application/json"
                                       "X-Penholder" PENHOLDER}
                             :body (json/generate-string payload)
                             :throw false})]
        {:ok? (= 200 (:status resp)) :status (:status resp)})
      (catch Exception e {:ok? false :body (.getMessage e)}))))

(defn http-get-edn [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER} :throw false})]
    (when (= 200 (:status resp))
      (edn/read-string (:body resp)))))

(defn fetch-of-type [hx-type label]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?type="
                             (java.net.URLEncoder/encode hx-type "UTF-8")))
        all (or (:hyperedges r) [])]
    (filter #(= label (get-in % [:hx/props :repo])) all)))

(defn fetch-all [hx-type]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?type="
                             (java.net.URLEncoder/encode hx-type "UTF-8")))]
    (or (:hyperedges r) [])))

;; ---------- helpers ----------

(defn real-eps [h]
  (remove #(str/starts-with? % "dir:") (:hx/endpoints h)))

(defn ns-of-vertex [qname]
  (when-let [i (str/last-index-of qname "/")]
    (subs qname 0 i)))

(defn build-graph-for-label [label]
  (let [vars (fetch-of-type "code/v05/var" label)
        cov  (fetch-of-type "code/v05/coverage" label)
        cal  (fetch-of-type "code/v05/calls" label)
        voc  (fetch-of-type "code/v05/vocabulary-use" label)
        var-qs (set (mapcat real-eps vars))
        edge-shape (fn [h]
                     (let [eps (real-eps h)]
                       {:src (first eps) :dst (second eps)}))]
    {:vars vars
     :var-qs var-qs
     :cov-edges (mapv edge-shape cov)
     :call-edges (mapv edge-shape cal)
     :voc-edges (mapv edge-shape voc)
     :ns-set (set (keep (fn [h] (get-in h [:hx/props :var/ns])) vars))}))

(defn connected-components [{:keys [var-qs cov-edges call-edges]}]
  (let [adj (reduce (fn [a {:keys [src dst]}]
                      (-> a (update src (fnil conj #{}) dst)
                            (update dst (fnil conj #{}) src)))
                    {} (concat cov-edges call-edges))
        seen (atom #{}) comps (atom [])]
    (doseq [n var-qs :when (not (@seen n))]
      (let [stack (atom [n]) comp (atom #{})]
        (while (seq @stack)
          (let [v (peek @stack)]
            (swap! stack pop)
            (when-not (@seen v)
              (swap! seen conj v)
              (swap! comp conj v)
              (doseq [u (adj v)] (swap! stack conj u)))))
        (swap! comps conj @comp)))
    @comps))

(defn ns-fingerprint [voc-edges]
  (reduce (fn [acc {:keys [src dst]}]
            (update acc src (fnil conj #{}) dst))
          {} voc-edges))

(defn component-fingerprint [components ns→terms]
  (into {}
        (map-indexed
         (fn [i comp]
           (let [nss (into #{} (keep ns-of-vertex comp))
                 fp  (into #{} (mapcat #(get ns→terms % #{}) nss))]
             [i {:size (count comp) :namespaces nss :terms fp}]))
         components)))

(defn jaccard [a b]
  (let [u (count (clojure.set/union a b))]
    (if (zero? u) 0.0 (double (/ (count (clojure.set/intersection a b)) u)))))

(defn cross-component-edges
  "Set of (c₁, c₂) pairs that have at least one call/coverage edge crossing them."
  [components edges]
  (let [vertex→comp (into {}
                          (mapcat (fn [i comp]
                                    (for [v comp] [v i]))
                                  (range)
                                  components))
        pairs (atom #{})]
    (doseq [{:keys [src dst]} edges
            :let [ci (get vertex→comp src)
                  cj (get vertex→comp dst)]
            :when (and ci cj (not= ci cj))]
      (swap! pairs conj #{ci cj}))
    @pairs))

;; ---------- signature 1: adapter shim that doesn't adapt ----------

(def adapter-name-tokens #{"adapter" "adapters" "bridge" "shim" "port" "ports"})

(defn signature-adapter-shim [{:keys [vars call-edges]} label]
  (let [adapter-vars (filter (fn [v]
                               (let [ns (get-in v [:hx/props :var/ns])
                                     parts (set (str/split (or ns "") #"\."))]
                                 (some adapter-name-tokens parts)))
                             vars)
        adapter-ns-set (into #{}
                             (keep #(get-in % [:hx/props :var/ns]) adapter-vars))
        adapter-qs (into #{} (mapcat real-eps adapter-vars))
        outgoing (group-by :src
                          (filter (fn [{:keys [src dst]}]
                                    (and (adapter-qs src) (not (adapter-qs dst))))
                                  call-edges))
        ;; A shim "doesn't adapt" if its outgoing-call-graph contains no
        ;; non-adapter targets (it only wires within the adapter family).
        ;; v0: report each adapter ns whose outgoing-cross-namespace count
        ;; is 0.
        per-ns (group-by (fn [v] (get-in v [:hx/props :var/ns])) adapter-vars)]
    (vec
     (for [[ns members] per-ns
           :let [member-qs (into #{} (mapcat real-eps members))
                 cross-edges (filter (fn [{:keys [src dst]}]
                                       (and (member-qs src)
                                            (not (member-qs dst))
                                            ;; skip same-ns edges
                                            (not= ns (some-> dst ns-of-vertex))))
                                     call-edges)
                 cross-target-nss (into #{} (keep #(some-> (:dst %) ns-of-vertex)
                                                   cross-edges))]
           :when (zero? (count cross-target-nss))]
       {:signature "adapter-shim-no-adapt"
        :severity (if (>= (count members) 5) :high :medium)
        :evidence-vertices (vec (map :var/qname (take 10 members)))
        :details {:adapter-ns ns
                  :member-count (count members)
                  :cross-target-ns-count 0}}))))

;; ---------- signature 2: work-around (drift-without-structure) ----------

(defn signature-work-around [graph theta label]
  (let [{:keys [voc-edges call-edges cov-edges]} graph
        comps (connected-components graph)
        ns→terms (ns-fingerprint voc-edges)
        fps (component-fingerprint comps ns→terms)
        crossing (cross-component-edges comps (concat call-edges cov-edges))
        ks (sort (keys fps))]
    (vec
     (for [a ks b ks :when (< a b)
           :let [overlap (jaccard (:terms (fps a)) (:terms (fps b)))
                 has-cross? (crossing #{a b})
                 sa (:size (fps a))
                 sb (:size (fps b))]
           :when (and (>= overlap theta)
                      (not has-cross?)
                      (>= (min sa sb) 5))]   ; ignore tiny components
       {:signature "work-around-drift"
        :severity (cond (>= overlap 0.5) :high
                        (>= overlap 0.2) :medium
                        :else :low)
        :evidence-vertices (vec (concat (take 5 (:namespaces (fps a)))
                                         (take 5 (:namespaces (fps b)))))
        :details {:c1 a :c2 b
                  :overlap overlap
                  :sizes [sa sb]
                  :shared-terms (vec (sort (clojure.set/intersection
                                             (:terms (fps a))
                                             (:terms (fps b)))))}}))))

;; ---------- signature 3: concept used without definition ----------

(defn signature-concept-undefined [{:keys [voc-edges]} label]
  (let [defines-all (fetch-all "code/v05/term-defines")
        defined-terms (set (map (fn [h] (second (real-eps h))) defines-all))
        used-terms (group-by :dst voc-edges)
        undefined-uses (filter (fn [[term _]] (not (defined-terms term)))
                                used-terms)]
    (vec
     (for [[term uses] undefined-uses
           :let [users (vec (sort (set (map :src uses))))]]
       {:signature "concept-used-without-definition"
        :severity (if (>= (count users) 5) :medium :low)
        :evidence-vertices users
        :details {:term term :usage-count (count uses) :user-ns-count (count users)}}))))

;; ---------- signature 4: completion rot ----------

(defn signature-completion-rot [label]
  (let [;; A label that has a watcher-event of source "heartbeat" but
        ;; no fresh ingest events in the last 24h is "rotting" (substrate
        ;; is queried but not refreshed). v0 simplification: emit a
        ;; signature when total :var count for the label is < 10 (clearly
        ;; under-ingested) — proxies for "label exists but its claimed
        ;; deliverables are not all present in the substrate".
        n-vars (count (fetch-of-type "code/v05/var" label))
        n-tests (count (fetch-of-type "code/v05/test" label))
        n-coverage (count (fetch-of-type "code/v05/coverage" label))
        n-calls (count (fetch-of-type "code/v05/calls" label))]
    (vec
     (cond-> []
       (< n-vars 10)
       (conj {:signature "completion-rot"
              :severity :high
              :evidence-vertices [label]
              :details {:reason "label has fewer than 10 :var hyperedges"
                        :n-vars n-vars :n-tests n-tests
                        :n-coverage n-coverage :n-calls n-calls}})))))

;; ---------- signature 5 (v0.5): coverage-retreat ----------
;;
;; Walks the per-label commit chain (commits ordered by :precedes) and
;; computes a streaming approximation of "coverage % over time":
;;   At commit c, accumulated-vars = vars touched by :edits up through c.
;;   At commit c, accumulated-covered = those vars with at least one
;;     incident :coverage edge in current substrate.
;;   coverage%(c) = covered/total
;; Emits one signature per contiguous monotonic-decreasing window of
;; length ≥ 5 with total drop ≥ 5 percentage points.
;;
;; This is a v0.5 approximation: the substrate doesn't store per-commit
;; coverage state, so we use current-state coverage with historical var
;; introduction order. A var introduced at commit c is "uncovered at c"
;; if it has no current :coverage edge, even if a covering test was
;; added later. v1 would re-project per commit to get the historical
;; truth.

(defn fetch-of-type-with-real-eps [hx-type label]
  (let [hes (fetch-of-type hx-type label)]
    (mapv (fn [h]
            (let [eps (real-eps h)
                  ts (get-in h [:hx/props :timestamp])]
              {:hx h :eps eps :ts ts
               :sha (first eps) :var-qname (second eps)}))
          hes)))

(defn signature-coverage-retreat [label]
  (let [commits (fetch-of-type "code/v05/commit" label)
        ;; Order commits by :timestamp prop (Unix seconds).
        ordered (sort-by (fn [c] (get-in c [:hx/props :timestamp])) commits)
        sha->idx (into {} (map-indexed (fn [i c] [(first (real-eps c)) i]) ordered))
        edits (fetch-of-type "code/v05/edits" label)
        cov-edges (fetch-of-type "code/v05/coverage" label)
        covered-qnames (set (map (fn [e] (second (real-eps e))) cov-edges))
        ;; var → earliest commit-idx where :edits touched it.
        var-introduced-at
        (reduce (fn [acc e]
                  (let [eps (real-eps e)
                        sha (first eps)
                        v (second eps)
                        idx (sha->idx sha)]
                    (if idx
                      (update acc v (fn [old] (if old (min old idx) idx)))
                      acc)))
                {} edits)
        n-commits (count ordered)
        ;; Build per-commit coverage% time-series.
        series (loop [i 0 acc-vars #{} acc-cov 0 series []]
                 (if (>= i n-commits)
                   series
                   (let [new-vars (set (keep (fn [[v idx]] (when (= idx i) v))
                                              var-introduced-at))
                         all-vars (clojure.set/union acc-vars new-vars)
                         new-covered (count (filter covered-qnames new-vars))
                         total-covered (+ acc-cov new-covered)
                         pct (if (zero? (count all-vars))
                               0.0
                               (* 100.0 (/ total-covered (double (count all-vars)))))]
                     (recur (inc i)
                            all-vars total-covered
                            (conj series {:idx i
                                          :sha (first (real-eps (nth ordered i)))
                                          :n-vars (count all-vars)
                                          :n-covered total-covered
                                          :pct pct})))))
        ;; Detect monotonic-decreasing windows of length ≥ 5 with total
        ;; drop ≥ 5 percentage points.
        windows (loop [i 0 start nil prev-pct nil out []]
                  (if (>= i (count series))
                    (if (and start (>= (- i start) 5))
                      (conj out [start (dec i)])
                      out)
                    (let [pct (:pct (nth series i))]
                      (cond
                        (nil? prev-pct)
                        (recur (inc i) i pct out)
                        (<= pct prev-pct)
                        (recur (inc i) start pct out)
                        :else
                        (let [span [start (dec i)]
                              run-len (- i start)
                              start-pct (:pct (nth series start))
                              end-pct (:pct (nth series (dec i)))
                              drop (- start-pct end-pct)]
                          (recur (inc i) i pct
                                 (if (and (>= run-len 5) (>= drop 5.0))
                                   (conj out span) out)))))))]
    (vec
     (for [[a b] windows
           :let [s (nth series a)
                 e (nth series b)
                 drop (- (:pct s) (:pct e))]
           ;; Suppress "0%→0%" flat windows — they're "never covered",
           ;; not "coverage retreat". Require start-pct > 0 AND strict
           ;; non-zero drop.
           :when (and (> (:pct s) 0.0) (> drop 0.0))]
       {:signature "coverage-retreat"
        :severity (cond (>= drop 20) :high
                        (>= drop 10) :medium
                        :else :low)
        :evidence-vertices [(:sha s) (:sha e)]
        :details {:from-commit (:sha s) :to-commit (:sha e)
                  :commit-span (- b a -1)
                  :start-pct (:pct s) :end-pct (:pct e)
                  :drop drop
                  :n-vars-start (:n-vars s) :n-vars-end (:n-vars e)}}))))

;; ---------- signature 6 (v0.5): concept-introduced-without-attachment ----------
;;
;; Per-label: terms defined in some vocab doc but never used by any
;; namespace in this label. Operationally: vocabulary that the label
;; *should* be aware of (or has been told about) but isn't.

(defn signature-concept-without-attachment [{:keys [voc-edges]} label]
  (let [defines-all (fetch-all "code/v05/term-defines")
        defined-terms (set (map (fn [h] (second (real-eps h))) defines-all))
        used-terms-here (set (map :dst voc-edges))
        unused (clojure.set/difference defined-terms used-terms-here)]
    (when (seq defined-terms)
      ;; Emit one signature with the full list (not 140 individual
      ;; signatures per label). Lower severity if many terms unused
      ;; (likely operator hasn't read those vocab docs); higher if
      ;; few are unused (suggests deliberate skipping).
      (let [n-defined (count defined-terms)
            n-unused (count unused)
            ratio (double (/ n-unused (max 1 n-defined)))]
        [{:signature "concept-introduced-without-attachment"
          :severity (cond (< ratio 0.1) :high     ; almost all used; the few unused are surprising
                          (< ratio 0.5) :medium
                          :else :low)             ; most unused; not surprising
          :evidence-vertices (vec (take 20 (sort unused)))
          :details {:n-defined n-defined
                    :n-used (- n-defined n-unused)
                    :n-unused n-unused
                    :ratio-unused ratio
                    :sample-unused (vec (take 20 (sort unused)))}}]))))

;; ---------- emission ----------

(defn emit-signature! [{:keys [signature severity evidence-vertices details]} label]
  (let [content-key (str signature ":" label ":"
                          (subs (str (hash details)) 0 (min 10 (count (str (hash details))))))
        sig-id (str "sig:" content-key)]
    (post-hyperedge!
     "code/v05/satisficing-signature"
     [sig-id signature]
     ["v05" "phase-5" label signature]
     {"repo" label
      "phase" 5
      "signature" signature
      "severity" (name severity)
      "ts" (System/currentTimeMillis)
      "evidence-vertices" evidence-vertices
      "details" details})))

;; ---------- main ----------

(defn detect-and-emit-for-label [label theta]
  (let [graph (build-graph-for-label label)
        sigs (concat
              (signature-coverage-retreat label)
              (signature-concept-without-attachment graph label)
              (signature-adapter-shim graph label)
              (signature-work-around graph theta label)
              (signature-concept-undefined graph label)
              (signature-completion-rot label))]
    (println (format "[%s] %d signatures detected" label (count sigs)))
    (doseq [s sigs]
      (emit-signature! s label)
      (println (format "  %-30s %s  %s"
                       (:signature s)
                       (name (:severity s))
                       (pr-str (:details s)))))
    (count sigs)))

(defn parse-args [argv]
  (loop [a argv opts {:labels [] :theta 0.05}]
    (cond
      (empty? a) opts
      (= "--label" (first a)) (recur (drop 2 a) (update opts :labels conj (second a)))
      (= "--all-labels" (first a)) (recur (rest a) (assoc opts :all? true))
      (= "--theta" (first a)) (recur (drop 2 a) (assoc opts :theta (parse-double (second a))))
      :else (recur (rest a) opts))))

(defn discover-active-labels []
  (let [vars (fetch-all "code/v05/var")
        labels (set (keep #(get-in % [:hx/props :repo]) vars))]
    (vec (sort labels))))

(defn -main [& argv]
  (let [{:keys [labels all? theta]} (parse-args argv)
        targets (if all? (discover-active-labels) labels)]
    (when (empty? targets)
      (println "ERROR: pass --label <l> or --all-labels") (System/exit 2))
    (println "[phase_5_signatures] θ =" theta " over" (count targets) "labels")
    (println)
    (let [total (atom 0)]
      (doseq [l targets]
        (swap! total + (detect-and-emit-for-label l theta)))
      (println)
      (println "=== TOTAL signatures emitted:" @total))))

(apply -main *command-line-args*)
