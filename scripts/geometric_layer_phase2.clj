#!/usr/bin/env bb
;; substrate-2 phase-2: geometric layer (T, ∇T, ΔT, drift) computed by
;; querying the live futon1a substrate. The geometric quantities are
;; *derived*, not stored. This script is the read-side reference impl;
;; phase 4 (watcher) will move equivalent computations into the JVM.
;;
;; Usage:
;;   bb geometric_layer_phase2.clj [--label tag] [--out file.edn]
;;
;; Reads (from futon1a HTTP API at FUTON1A_URL, default :7071):
;;   code/v05/var, code/v05/test, code/v05/namespace, code/v05/term
;;   code/v05/calls, code/v05/coverage, code/v05/vocabulary-use,
;;   code/v05/term-defines, code/v05/contains
;; All filtered by :hx/props :repo == --label.
;;
;; Computes (definitions per E-cross-prototype-geometry.md §"v0 first-pass"):
;;   T(var)  = 1 if no incident :coverage edge else 0
;;   T(test) = 0
;;   ∇T(e)   = T(target) − T(source)
;;   ΔT(v)   = Σ_{e: target=v} ∇T(e) − Σ_{e: source=v} ∇T(e)
;;   components: undirected over :coverage ∪ :calls
;;   drift(C₁,C₂) = Jaccard(V(C₁), V(C₂)) over per-ns vocab fingerprints

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.set]
         '[babashka.http-client :as http]
         '[cheshire.core :as json])

(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))

(defn http-get-edn [url]
  (let [resp (http/get url {:headers {"X-Penholder" PENHOLDER} :throw false})]
    (if (= 200 (:status resp))
      (edn/read-string (:body resp))
      (throw (ex-info "HTTP non-200"
                      {:url url :status (:status resp) :body (:body resp)})))))

(defn fetch-of-type
  "Pull all hyperedges of a given type, optionally filtered by :repo prop."
  [hx-type label]
  (let [r (http-get-edn (str FUTON1A "/api/alpha/hyperedges?type="
                             (java.net.URLEncoder/encode hx-type "UTF-8")))
        all (or (:hyperedges r) [])]
    (if label
      (filter #(= label (get-in % [:hx/props :repo])) all)
      all)))

;; ---------- geometric layer ----------

(defn build-graph
  "Pull substrate, build in-memory representation suitable for T/∇/Δ."
  [label]
  (let [vars (fetch-of-type "code/v05/var" label)
        tests (fetch-of-type "code/v05/test" label)
        nss (fetch-of-type "code/v05/namespace" label)
        terms (fetch-of-type "code/v05/term" label)
        cov (fetch-of-type "code/v05/coverage" label)
        cal (fetch-of-type "code/v05/calls" label)
        voc (fetch-of-type "code/v05/vocabulary-use" label)
        contains (fetch-of-type "code/v05/contains" label)
        var-qs (set (mapcat :hx/endpoints vars))
        test-qs (set (mapcat :hx/endpoints tests))
        ns-set (set (mapcat :hx/endpoints nss))
        term-set (set (mapcat :hx/endpoints terms))
        ;; var-qname -> ns
        var->ns (into {} (map (fn [h]
                                (let [eps (:hx/endpoints h)]
                                  [(first eps) (get-in h [:hx/props :var/ns])]))
                              vars))
        ;; Edges may have a 3rd synthetic endpoint `dir:src→dst` for stable-ID
        ;; disambiguation (see E-substrate-2-directed-edge-id.md). The first
        ;; two endpoints remain (src, dst) by convention.
        edge-shape (fn [h] {:src (first (:hx/endpoints h))
                            :dst (second (:hx/endpoints h))})]
    {:vars       vars
     :var-qs     var-qs
     :tests      tests
     :test-qs    test-qs
     :ns-set     ns-set
     :term-set   term-set
     :var->ns    var->ns
     :cov-edges  (mapv edge-shape cov)
     :call-edges (mapv edge-shape cal)
     :voc-edges  (mapv edge-shape voc)
     :contains-edges (mapv edge-shape contains)}))

(defn compute-T [{:keys [var-qs cov-edges]}]
  (let [covered (set (map :dst cov-edges))]
    (into {} (for [q var-qs]
               [q (if (covered q) 0 1)]))))

(defn compute-grad [edges T]
  (mapv (fn [e]
          (assoc e :grad (- (get T (:dst e) 0) (get T (:src e) 0))))
        edges))

(defn compute-laplacian [edges-with-grad]
  (reduce (fn [acc e]
            (-> acc
                (update (:dst e) (fnil + 0) (:grad e))
                (update (:src e) (fnil + 0) (- (:grad e)))))
          {} edges-with-grad))

(defn connected-components
  "Undirected components over coverage ∪ calls, on var/test endpoints."
  [{:keys [var-qs test-qs cov-edges call-edges]}]
  (let [nodes (clojure.set/union var-qs test-qs)
        adj (reduce (fn [a {:keys [src dst]}]
                      (-> a
                          (update src (fnil conj #{}) dst)
                          (update dst (fnil conj #{}) src)))
                    {}
                    (concat cov-edges call-edges))
        seen (atom #{})
        comps (atom [])]
    (doseq [n nodes :when (not (@seen n))]
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

(defn ns-of-vertex [qname]
  (when-let [i (str/last-index-of qname "/")]
    (subs qname 0 i)))

(defn ns-fingerprint
  "Per-namespace vocab fingerprint from voc-edges (ns → term)."
  [voc-edges]
  (reduce (fn [acc {:keys [src dst]}]
            (update acc src (fnil conj #{}) dst))
          {} voc-edges))

(defn component-fingerprint [components ns→terms]
  (into {}
        (map-indexed
         (fn [i comp]
           (let [nss (into #{} (keep ns-of-vertex comp))
                 fp  (into #{} (mapcat #(get ns→terms % #{}) nss))]
             [i {:size (count comp)
                 :namespaces nss
                 :terms fp}]))
         components)))

(defn jaccard [a b]
  (let [u (count (clojure.set/union a b))]
    (if (zero? u) 0.0 (double (/ (count (clojure.set/intersection a b)) u)))))

(defn pairwise-overlap [fingerprints]
  (let [ks (sort (keys fingerprints))]
    (for [a ks b ks :when (< a b)]
      {:c1 a :c2 b
       :overlap (jaccard (:terms (fingerprints a))
                         (:terms (fingerprints b)))
       :shared (clojure.set/intersection
                (:terms (fingerprints a))
                (:terms (fingerprints b)))})))

(defn report [label {:keys [vars tests var-qs test-qs ns-set term-set
                             cov-edges call-edges voc-edges contains-edges]
                      :as graph}]
  (let [T (compute-T graph)
        all (concat cov-edges call-edges)
        grad (compute-grad all T)
        lap (compute-laplacian grad)
        var-set var-qs
        top-pos (->> lap (filter #(var-set (key %)))
                     (sort-by (comp - val))
                     (filter #(pos? (val %)))
                     (take 15))
        top-neg (->> lap (filter #(var-set (key %)))
                     (sort-by val)
                     (filter #(neg? (val %)))
                     (take 15))
        comps (connected-components graph)
        ns→terms (ns-fingerprint voc-edges)
        fps (component-fingerprint comps ns→terms)
        overlaps (pairwise-overlap fps)
        n-tense (count (filter #(= 1 (val %)) T))]
    {:label label
     :counts {:vars (count var-qs)
              :tests (count test-qs)
              :namespaces (count ns-set)
              :terms (count term-set)
              :coverage-edges (count cov-edges)
              :call-edges (count call-edges)
              :vocab-edges (count voc-edges)
              :contains-edges (count contains-edges)
              :unpaired-vars n-tense
              :paired-vars (- (count var-qs) n-tense)
              :coverage-pct (when (pos? (count var-qs))
                              (* 100.0 (/ (- (count var-qs) n-tense)
                                          (double (count var-qs)))))
              :components (count comps)}
     :top-+deltaT (mapv (fn [[q d]] {:var q :delta-T d}) top-pos)
     :top--deltaT (mapv (fn [[q d]] {:var q :delta-T d}) top-neg)
     :nonzero-laplacian-count (count (filter (comp not zero? val) lap))
     :components-summary
     (->> fps (sort-by (comp - :size second)) (take 10)
          (mapv (fn [[i {:keys [size namespaces terms]}]]
                  {:component i
                   :size size
                   :ns-count (count namespaces)
                   :sample-namespaces (vec (take 5 namespaces))
                   :n-terms (count terms)
                   :sample-terms (vec (take 12 (sort terms)))})))
     :drift-hotspots
     (when (seq overlaps)
       (->> overlaps
            (filter #(>= (:overlap %) 0.05))
            (sort-by (comp - :overlap))
            (take 10)
            (mapv (fn [{:keys [c1 c2 overlap shared]}]
                    {:c1 c1 :c2 c2
                     :overlap overlap
                     :shared-terms (vec (sort shared))}))))}))

(defn parse-args [argv]
  (loop [a argv opts {:label nil :out nil}]
    (cond
      (empty? a) opts
      (= "--label" (first a)) (recur (drop 2 a) (assoc opts :label (second a)))
      (= "--out"   (first a)) (recur (drop 2 a) (assoc opts :out (second a)))
      :else (recur (rest a) opts))))

(defn -main [& argv]
  (let [{:keys [label out]} (parse-args argv)]
    (when-not label
      (println "ERROR: --label is required (e.g. futon2-phase1)") (System/exit 2))
    (println "[geometric_layer_phase2] reading substrate-2 substrate from" FUTON1A)
    (println "  label:" label)
    (let [graph (build-graph label)
          rpt (report label graph)]
      (if out
        (do (spit out (with-out-str (clojure.pprint/pprint rpt)))
            (println "Wrote" out))
        (clojure.pprint/pprint rpt)))))

(apply -main *command-line-args*)
