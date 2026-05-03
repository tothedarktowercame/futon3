#!/usr/bin/env bb
;; v0 codebase hypergraph projection for E-cross-prototype-geometry.md
;;
;; Usage:
;;   bb v0_codebase_hypergraph.clj <root-dir> [--out <path>] [--label <tag>]
;;
;; Vertex types: :var (defn/def family), :test (deftest)
;; Edge types:
;;   :coverage (test → var, body-symbol reference)
;;   :calls    (var  → var, body-symbol reference)
;;
;; Per E-math-prototype-pilot.md §"Tension scalar demo":
;;   T(var)  = 1 if no incident :coverage edge else 0
;;   T(test) = 0
;;   ∇T(e)   = T(target) − T(source)
;;   ΔT(v)   = Σ_{e ∋ v} ∇T(e)  (signed incoming − outgoing)

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[babashka.fs :as fs]
         '[clojure.walk :as walk]
         '[clojure.set]
         '[cheshire.core :as json])

(load-file (str (fs/parent *file*) "/elisp_projection.clj"))
(load-file (str (fs/parent *file*) "/python_projection.clj"))
(load-file (str (fs/parent *file*) "/flexiarg_projection.clj"))

(def src-exts #{"clj" "cljs" "cljc"})
(def excluded-path-segments
  #{".git" ".venv" "venv" "node_modules" "__pycache__" ".mypy_cache" ".pytest_cache"})
(def def-forms #{'defn 'defn- 'def 'defmulti 'defmethod 'defprotocol 'defrecord 'deftype})

(defn excluded-path? [path]
  (let [parts (set (map str (fs/components path)))]
    (boolean (seq (clojure.set/intersection excluded-path-segments parts)))))

(defn pruned-file-seq [root]
  (letfn [(children [^java.io.File f]
            (when (.isDirectory f)
              (->> (.listFiles f)
                   (remove #(or (and (.isDirectory ^java.io.File %)
                                     (excluded-path? (.getPath ^java.io.File %)))
                                (java.nio.file.Files/isSymbolicLink
                                 (.toPath ^java.io.File %)))))))]
    (->> (tree-seq #(.isDirectory ^java.io.File %) children (io/file root))
         (filter #(.isFile ^java.io.File %)))))

;; ---------- vocabulary extraction (v0.5) ----------

(def vocab-stopwords
  #{"the" "and" "or" "of" "to" "for" "in" "on" "at" "by" "with"
    "from" "is" "are" "was" "were" "be" "been" "being"
    "this" "that" "these" "those" "what" "where" "when" "how"
    "which" "who" "an" "as" "if" "but" "not" "no" "yes"
    "key insight" "next steps" "current status" "summary"
    "before" "after" "during" "within" "across" "between"
    "domain" "system" "code" "structure" "process" "function"
    "module" "package" "service" "platform"
    "loop" "step" "phase" "state" "time" "value" "type"
    "result" "output" "input" "data" "test" "tests"})

(defn clean-term [s]
  (-> s
      (str/replace #"\s*\([^)]*\)\s*" "")
      (str/replace #"^[\s*#\-•]+" "")
      (str/replace #"\s+" " ")
      str/trim
      str/lower-case))

(defn good-term? [t]
  (and (string? t)
       (let [n (count t)]
         (and (>= n 3) (<= n 40)))
       (re-matches #"[a-z][a-z0-9 \-/&]*" t)
       (not (vocab-stopwords t))
       (not (re-matches #"^[0-9]+$" t))
       ;; aggressive filter: terms with imperative-verb starters or
       ;; clearly-prose fragments don't make good terms
       (not (re-find #"^(apply|check|combine|bias|blend|audit|update|select|compute|emit|find|use|get|put|drop|skip)\b" t))
       ;; ≤4 tokens
       (<= (count (str/split t #"\s+")) 4)))

(defn extract-vocab-terms
  "Returns sorted-set of cleaned terms from a vocab markdown doc.
   Sources: section headings, **bolded** phrases, *italic* phrases."
  [path]
  (let [text (slurp path)
        from-headings (->> (str/split-lines text)
                           (keep #(when-let [m (re-find #"^#{2,}\s+(.+?)\s*$" %)]
                                    (clean-term (second m)))))
        from-bold     (map (comp clean-term second)
                           (re-seq #"\*\*([^*\n]+?)\*\*" text))
        from-italic   (map (comp clean-term second)
                           (re-seq #"(?<!\*)\*([^*\n]{2,40})\*(?!\*)" text))
        cands         (concat from-headings from-bold from-italic)]
    (into (sorted-set) (filter good-term? cands))))

(defn build-term-set
  "vocab-paths → {:terms #{...} :term→docs {term [doc ...]}}"
  [vocab-paths]
  (reduce (fn [acc p]
            (let [ts (extract-vocab-terms p)]
              (reduce (fn [a t]
                        (-> a
                            (update :terms conj t)
                            (update-in [:term→docs t] (fnil conj []) p)))
                      acc ts)))
          {:terms (sorted-set) :term→docs {}}
          vocab-paths))

(defn term-pattern
  "Compile a regex that matches any term in `terms` as a whole-token sequence
   (case-insensitive). For multi-word terms allow whitespace runs of any length."
  [terms]
  (when (seq terms)
    (let [escape (fn [t]
                   (-> t
                       (str/replace #"([.\\+*?\[\]^$|(){}])" "\\\\$1")
                       (str/replace #"\s+" "\\\\s+")))
          alt    (str/join "|" (map escape (sort-by (comp - count) terms)))]
      (re-pattern (str "(?i)\\b(" alt ")\\b")))))

(defn- normalise-term-hit
  "term-pattern allows `\\s+` between tokens, so captured groups may carry
   runs of whitespace not present in the canonical term-set. Collapse to
   single spaces to match `extract-vocab-terms` output."
  [s]
  (-> s str/lower-case (str/replace #"\s+" " ") str/trim))

(defn scan-file-for-terms [path term-rx]
  (when term-rx
    (let [text (slurp path)
          hits (re-seq term-rx text)]
      (into #{} (map (comp normalise-term-hit
                           #(if (vector? %) (second %) %)) hits)))))

(defn collect-vocab-edges
  "For each source-or-doc file in `paths`, scan for terms; emit ns→term
   :vocabulary-use edges where ns is the namespace defined in the file (or
   the file's relative path if it's not a clj-source-with-ns)."
  [files-with-ns term-rx]
  (when term-rx
    (mapcat (fn [{:keys [ns path]}]
              (let [terms (scan-file-for-terms path term-rx)]
                (for [t terms]
                  {:edge/type   :vocabulary-use
                   :edge/source ns
                   :edge/target (str ":term/" t)})))
            files-with-ns)))

(defn read-forms [^java.io.File f]
  (with-open [pbr (java.io.PushbackReader. (io/reader f))]
    (binding [*default-data-reader-fn* (fn [_t v] v)]
      (loop [acc []]
        (let [form (try (read {:read-cond :allow :features #{:clj :cljs} :eof ::eof} pbr)
                        (catch Exception _ ::eof))]
          (if (= form ::eof) acc (recur (conj acc form))))))))

(defn ns-form [forms]
  (some #(when (and (seq? %) (= 'ns (first %))) %) forms))

(defn parse-requires
  "Returns map alias-symbol → ns-symbol from an ns form."
  [ns-form]
  (let [reqs (some (fn [c] (when (and (seq? c) (= :require (first c))) (rest c)))
                   ns-form)
        flat (mapcat (fn [r]
                       (cond
                         (symbol? r) [[r r]]
                         (vector? r)
                         (let [n (first r) opts (apply hash-map (rest r))]
                           (if-let [a (:as opts)] [[a n]] [[n n]]))
                         :else []))
                     reqs)]
    (into {} flat)))

(defn defn-body
  "Strip name+docstring+attr-map+arity-list to leave the body forms (for symbol scan)."
  [form]
  (drop 2 form))

(defn collect-symbols [body]
  (let [syms (atom #{})]
    (walk/postwalk
     (fn [x] (when (symbol? x) (swap! syms conj x)) x)
     body)
    @syms))

(defn extract-defs
  "Returns {:vars [...] :tests [...]} for one file. Each entry carries body-syms
   for later edge resolution."
  [ns-sym aliases is-test? forms]
  (let [vars  (atom [])
        tests (atom [])]
    (doseq [f forms :when (and (seq? f) (symbol? (first f)))]
      (let [head (first f)]
        (cond
          (= head 'deftest)
          (let [tname (second f)]
            (when (symbol? tname)
              (swap! tests conj
                     {:vertex/type :test
                      :test/ns     (str ns-sym)
                      :test/name   (str tname)
                      :test/qname  (str ns-sym "/" tname)
                      :test/syms   (collect-symbols (defn-body f))})))
          (def-forms head)
          (let [vname (second f)
                rest-f (drop 2 f)
                docstr (when (string? (first rest-f)) (first rest-f))]
            (when (symbol? vname)
              (swap! vars conj
                     {:vertex/type :var
                      :var/ns      (str ns-sym)
                      :var/name    (str vname)
                      :var/qname   (str ns-sym "/" vname)
                      :var/kind    (str head)
                      :var/has-doc (some? docstr)
                      :var/syms    (collect-symbols (defn-body f))}))))))
    {:vars (if is-test? [] @vars) :tests (if is-test? @tests []) :aliases aliases}))

(defn collect [root]
  (let [files (pruned-file-seq root)
        py-files (->> files
                      (map #(.getPath ^java.io.File %))
                      (filter #(python-projection/src-exts (fs/extension %))))
        py-projections (when (seq py-files)
                         (python-projection/collect-files py-files))
        out   (atom {:vars [] :tests [] :files 0 :ns-set #{}
                     :ns→aliases {}
                     :ns→file {}})]
    (doseq [f files]
      (let [path (.getPath ^java.io.File f)
            ext  (fs/extension path)]
        (cond
          (src-exts ext)
          (let [forms (read-forms f)
                nf    (ns-form forms)
                nsym  (when nf (second nf))]
            (when nsym
              (swap! out update :ns-set conj (str nsym))
              (swap! out update :files inc)
              (let [is-test? (or (str/includes? path "/test/")
                                 (str/includes? path "/tests/")
                                 (str/ends-with? path "_test.clj")
                                 (str/ends-with? path "_test.cljs")
                                 (str/ends-with? path "_test.cljc")
                                 (str/ends-with? path ".spec.ts"))
                    aliases  (parse-requires nf)
                    {:keys [vars tests]} (extract-defs nsym aliases is-test? forms)]
                (swap! out update :vars  into vars)
                (swap! out update :tests into tests)
                (swap! out assoc-in [:ns→aliases (str nsym)] aliases)
                (swap! out assoc-in [:ns→file   (str nsym)] path))))

          (elisp-projection/src-exts ext)
          (let [{:keys [ns aliases vars tests]} (elisp-projection/collect-file path)]
            (swap! out update :ns-set conj ns)
            (swap! out update :files inc)
            (swap! out update :vars into vars)
            (swap! out update :tests into tests)
            (swap! out assoc-in [:ns→aliases ns] aliases)
            (swap! out assoc-in [:ns→file ns] path))

          (python-projection/src-exts ext)
          (let [{:keys [ns aliases vars tests]} (get py-projections path)]
            (swap! out update :ns-set conj ns)
            (swap! out update :files inc)
            (swap! out update :vars into vars)
            (swap! out update :tests into tests)
            (swap! out assoc-in [:ns→aliases ns] aliases)
            (swap! out assoc-in [:ns→file ns] path))

          (flexiarg-projection/src-exts ext)
          (when-let [{:keys [ns aliases vars tests]} (flexiarg-projection/collect-file path)]
            (swap! out update :ns-set conj ns)
            (swap! out update :files inc)
            (swap! out update :vars into vars)
            (swap! out update :tests into tests)
            (swap! out assoc-in [:ns→aliases ns] aliases)
            (swap! out assoc-in [:ns→file ns] path)))))
    @out))

(defn prefixed-elisp-ns [name known-nss]
  (->> known-nss
       (filter #(str/starts-with? name (str % "-")))
       (sort-by count >)
       first))

(defn connected-components
  "Undirected components over the call/coverage edge subset (var/test vertices)."
  [vars tests cov-edges call-edges]
  (let [nodes (into #{} (concat (map :var/qname vars) (map :test/qname tests)))
        adj (reduce (fn [a {:keys [edge/source edge/target]}]
                      (-> a
                          (update source (fnil conj #{}) target)
                          (update target (fnil conj #{}) source)))
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

(defn component-fingerprint
  "Map component → set of terms used by any ns in that component."
  [components ns→terms]
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

(defn pairwise-overlap [fingerprints]
  (let [ks (sort (keys fingerprints))]
    (for [a ks b ks :when (< a b)]
      {:c1 a :c2 b
       :overlap (jaccard (:terms (fingerprints a))
                         (:terms (fingerprints b)))
       :shared  (clojure.set/intersection
                 (:terms (fingerprints a))
                 (:terms (fingerprints b)))})))

(defn resolve-symbol
  "Try to resolve a body-symbol s to a known var qname, given:
     - current ns name (string)
     - aliases (map alias-symbol → full ns symbol) for this ns
     - all known vars indexed by ns→{name→qname}"
  [s cur-ns aliases by-ns]
  (let [nm  (name s)
        nsp (namespace s)]
    (cond
      ;; same-ns unqualified
      (and (nil? nsp)
           (get-in by-ns [cur-ns nm]))
      (get-in by-ns [cur-ns nm])
      ;; python-style imported symbol in local namespace
      (and (nil? nsp) aliases)
      (let [a-sym (symbol nm)
            target-ns (some-> (get aliases a-sym) str)]
        (when (and target-ns (get-in by-ns [target-ns nm]))
          (get-in by-ns [target-ns nm])))
      ;; elisp-style unqualified cross-file reference by feature prefix
      (nil? nsp)
      (when-let [target-ns (prefixed-elisp-ns nm (keys by-ns))]
        (get-in by-ns [target-ns nm]))
      ;; alias-qualified
      (and nsp aliases)
      (let [a-sym (symbol nsp)
            target-ns (some-> (get aliases a-sym) str)]
        (when (and target-ns (get-in by-ns [target-ns nm]))
          (get-in by-ns [target-ns nm])))
      ;; fully-qualified
      (and nsp (get-in by-ns [nsp nm]))
      (get-in by-ns [nsp nm]))))

(defn build-edges [{:keys [vars tests ns→aliases]}]
  (let [by-ns (reduce (fn [acc v]
                        (assoc-in acc [(:var/ns v) (:var/name v)] (:var/qname v)))
                      {} vars)
        cov (atom [])
        cal (atom [])]
    ;; coverage: test/syms → var
    (doseq [t tests
            s (:test/syms t)
            :let [aliases (get ns→aliases (:test/ns t))
                  qn (resolve-symbol s (:test/ns t) aliases by-ns)]
            :when (and qn (not= qn (:test/qname t)))]
      (swap! cov conj {:edge/type :coverage
                       :edge/source (:test/qname t)
                       :edge/target qn}))
    ;; calls: var/syms → var (other than self)
    (doseq [v vars
            s (:var/syms v)
            :let [aliases (get ns→aliases (:var/ns v))
                  qn (resolve-symbol s (:var/ns v) aliases by-ns)]
            :when (and qn (not= qn (:var/qname v)))]
      (swap! cal conj {:edge/type :calls
                       :edge/source (:var/qname v)
                       :edge/target qn}))
    ;; dedupe
    {:coverage (vec (distinct @cov))
     :calls    (vec (distinct @cal))}))

(defn compute-T [vars cov-edges]
  (let [covered (set (map :edge/target cov-edges))]
    (into {} (for [v vars]
               [(:var/qname v) (if (covered (:var/qname v)) 0 1)]))))

(defn compute-grad [edges T]
  (mapv (fn [e]
          (assoc e :edge/grad
                 (- (get T (:edge/target e) 0)
                    (get T (:edge/source e) 0))))
        edges))

(defn compute-laplacian
  "ΔT(v) = Σ_{e: target=v} ∇T(e) − Σ_{e: source=v} ∇T(e).
   This is the divergence of the gradient: net flow into v."
  [edges-with-grad]
  (reduce (fn [acc e]
            (-> acc
                (update (:edge/target e) (fnil + 0) (:edge/grad e))
                (update (:edge/source e) (fnil + 0) (- (:edge/grad e)))))
          {} edges-with-grad))

(defn report [opts {:keys [vars tests cov-edges call-edges vocab-edges
                            T lap files ns-set components fingerprints overlaps
                            term-count]}]
  (let [n-vars (count vars)
        n-cov  (count cov-edges)
        n-cal  (count call-edges)
        n-voc  (count (or vocab-edges []))
        n-tense (count (filter #(= 1 (val %)) T))
        var-set (set (map :var/qname vars))
        top-pos (take 15 (filter #(pos? (val %)) (sort-by (comp - val) (filter #(var-set (key %)) lap))))
        top-neg (take 15 (filter #(neg? (val %)) (sort-by val (filter #(var-set (key %)) lap))))
        ;; component summary, sorted by size desc
        comps-summary (->> (or fingerprints {})
                           (sort-by (comp - :size second))
                           (take 10)
                           (mapv (fn [[i {:keys [size namespaces terms]}]]
                                   {:component i
                                    :size size
                                    :ns-count (count namespaces)
                                    :sample-namespaces (vec (take 5 namespaces))
                                    :n-terms (count terms)
                                    :sample-terms (vec (take 12 (sort terms)))})))
        ;; flag drift hotspots: pairs of components with overlap > threshold
        drift-pairs (when (seq overlaps)
                      (->> overlaps
                           (filter #(>= (:overlap %) 0.05))
                           (sort-by (comp - :overlap))
                           (take 10)
                           (mapv (fn [{:keys [c1 c2 overlap shared]}]
                                   {:c1 c1 :c2 c2
                                    :overlap (* 1.0 overlap)
                                    :shared-terms (vec (sort shared))}))))]
    {:label opts
     :counts {:files files
              :namespaces (count ns-set)
              :vars n-vars
              :tests (count tests)
              :coverage-edges n-cov
              :call-edges n-cal
              :vocab-edges n-voc
              :term-count term-count
              :unpaired-vars n-tense
              :paired-vars (- n-vars n-tense)
              :coverage-pct (when (pos? n-vars) (* 100.0 (/ (- n-vars n-tense) (double n-vars))))
              :components (count (or components []))}
     :doc-discipline {:vars-with-doc (count (filter :var/has-doc vars))
                      :vars-without-doc (count (remove :var/has-doc vars))}
     :top-+deltaT (mapv (fn [[q d]] {:var q :delta-T d}) top-pos)
     :top--deltaT (mapv (fn [[q d]] {:var q :delta-T d}) top-neg)
     :nonzero-laplacian-count (count (filter (comp not zero? val) lap))
     :components-summary comps-summary
     :drift-hotspots drift-pairs}))

(defn parse-args [argv]
  (loop [args argv opts {:label "v0" :vocab []}]
    (cond
      (empty? args) opts
      (= "--out"   (first args)) (recur (drop 2 args) (assoc opts :out (second args)))
      (= "--label" (first args)) (recur (drop 2 args) (assoc opts :label (second args)))
      (= "--dump"  (first args)) (recur (drop 2 args) (assoc opts :dump (second args)))
      (= "--vocab" (first args)) (recur (drop 2 args) (update opts :vocab conj (second args)))
      (str/starts-with? (first args) "--") (recur (rest args) opts)
      :else (recur (rest args) (assoc opts :root (first args))))))

(defn -main [& argv]
  (let [opts (parse-args argv)
        root (or (:root opts) (System/getProperty "user.dir"))
        opts (assoc opts :root root)
        col  (collect root)
        {:keys [coverage calls]} (build-edges col)
        ;; vocabulary
        vocab-paths (:vocab opts)
        {:keys [terms] :as vbundle} (when (seq vocab-paths) (build-term-set vocab-paths))
        rx (when terms (term-pattern terms))
        ;; ns→terms scan (only over namespaces we know about)
        files-with-ns (for [[ns path] (:ns→file col)] {:ns ns :path path})
        vocab-edges (when rx
                      (collect-vocab-edges files-with-ns rx))
        ns→terms (into {} (for [[ns path] (:ns→file col)]
                            [ns (or (scan-file-for-terms path rx) #{})]))
        ;; geometry
        T (compute-T (:vars col) coverage)
        all-edges (concat coverage calls)
        grad (compute-grad all-edges T)
        lap  (compute-laplacian grad)
        ;; components + drift
        comps (connected-components (:vars col) (:tests col) coverage calls)
        fps (component-fingerprint comps ns→terms)
        overlaps (pairwise-overlap fps)
        result {:vars (:vars col) :tests (:tests col)
                :cov-edges coverage :call-edges calls
                :vocab-edges (or vocab-edges [])
                :T T :lap lap :files (:files col) :ns-set (:ns-set col)
                :components comps :fingerprints fps :overlaps overlaps
                :term-count (count (or terms #{}))}
        rpt (report opts result)]
    (when-let [d (:dump opts)]
      (spit d (with-out-str (clojure.pprint/pprint
                              {:vars (mapv #(dissoc % :var/syms) (:vars col))
                               :tests (mapv #(dissoc % :test/syms) (:tests col))
                               :cov-edges coverage :call-edges calls
                               :T T :lap lap}))))
    (if-let [out (:out opts)]
      (do (cond
            (str/ends-with? out ".json")
            (spit out (json/generate-string rpt {:pretty true}))
            :else (spit out (with-out-str (clojure.pprint/pprint rpt))))
          (println "Wrote" out))
      (clojure.pprint/pprint rpt))))

(apply -main *command-line-args*)
