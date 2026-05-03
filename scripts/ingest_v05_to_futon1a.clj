#!/usr/bin/env bb
;; substrate-2 phase-1 ingest: lift v0.5 edge taxonomy into futon1a.
;;
;; Usage:
;;   bb ingest_v05_to_futon1a.clj <root> [--vocab path]... [--label tag]
;;
;; Writes (one POST per edge/vertex):
;;   code/v05/var, code/v05/test, code/v05/namespace, code/v05/term,
;;   code/v05/doc                          (vertices)
;;   code/v05/calls, code/v05/coverage,
;;   code/v05/vocabulary-use, code/v05/term-defines, code/v05/contains  (edges)
;;
;; All hyperedges carry :hx/labels {:repo <label> :phase 1}.
;; Stable ID is computed by futon1a (hx:{type}:{sorted-endpoints}); ingest
;; is idempotent — re-running on same source state writes 0 new hyperedges.
;;
;; Inside-out invariants enforced before each write:
;; - L4 validation: the payload conforms to {:hx/type :hx/endpoints ...}.
;; - L3 authorization: X-Penholder header set.
;; - L2 endpoint resolution: every :calls/:coverage edge has both endpoints
;;   already written as :var/:test/:namespace; vocab-use targets are
;;   already written as :term. Vertices written before edges by phase order.
;; - L1 stable identity: substrate-1 :hx/id scheme reused.
;; - L0 durability: HTTP 200 from /api/alpha/hyperedge required.

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[babashka.fs :as fs]
         '[clojure.walk :as walk]
         '[clojure.set]
         '[babashka.http-client :as http]
         '[cheshire.core :as json])

(load-file (str (fs/parent *file*) "/elisp_projection.clj"))
(load-file (str (fs/parent *file*) "/python_projection.clj"))
(load-file (str (fs/parent *file*) "/flexiarg_projection.clj"))

(def FUTON1A (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))
(def PENHOLDER (or (System/getenv "FUTON1A_PENHOLDER") "api"))

;; Edge types whose direction is load-bearing. For these, append a synthetic
;; third endpoint `(str "dir:" src "→" dst)` so futon1a's stable-ID
;; (which sorts endpoints) distinguishes A→B from B→A. See
;; futon3/holes/excursions/E-substrate-2-directed-edge-id.md.
(def directed-types
  #{"code/v05/calls"
    "code/v05/coverage"
    "code/v05/vocabulary-use"
    "code/v05/term-defines"
    "code/v05/contains"})

(defn directed-endpoints
  "If hx-type is directed, append a synthetic direction marker so the
   stable-ID is distinct per direction. Otherwise return endpoints unchanged."
  [hx-type endpoints]
  (if (and (directed-types hx-type) (= 2 (count endpoints)))
    (conj (vec endpoints) (str "dir:" (first endpoints) "→" (second endpoints)))
    endpoints))

;; ---------- v0.5 projection (vendored from v0_codebase_hypergraph.clj) ----------

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

(defn read-forms [^java.io.File f]
  (with-open [pbr (java.io.PushbackReader. (io/reader f))]
    (binding [*default-data-reader-fn* (fn [_t v] v)]
      (loop [acc []]
        (let [form (try (read {:read-cond :allow :features #{:clj :cljs} :eof ::eof} pbr)
                        (catch Exception _ ::eof))]
          (if (= form ::eof) acc (recur (conj acc form))))))))

(defn ns-form [forms]
  (some #(when (and (seq? %) (= 'ns (first %))) %) forms))

(defn parse-requires [ns-form]
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

(defn defn-body [form] (drop 2 form))

(defn collect-symbols [body]
  (let [syms (atom #{})]
    (walk/postwalk (fn [x] (when (symbol? x) (swap! syms conj x)) x) body)
    @syms))

(defn extract-defs [ns-sym is-test? forms]
  (let [vars (atom []) tests (atom [])]
    (doseq [f forms :when (and (seq? f) (symbol? (first f)))]
      (let [head (first f)]
        (cond
          (= head 'deftest)
          (let [tname (second f)]
            (when (symbol? tname)
              (swap! tests conj
                     {:test/ns (str ns-sym) :test/name (str tname)
                      :test/qname (str ns-sym "/" tname)
                      :test/syms (collect-symbols (defn-body f))})))
          (def-forms head)
          (let [vname (second f)
                rest-f (drop 2 f)
                docstr (when (string? (first rest-f)) (first rest-f))]
            (when (symbol? vname)
              (swap! vars conj
                     {:var/ns (str ns-sym) :var/name (str vname)
                      :var/qname (str ns-sym "/" vname)
                      :var/kind (str head)
                      :var/has-doc (some? docstr)
                      :var/syms (collect-symbols (defn-body f))}))))))
    {:vars (if is-test? [] @vars) :tests (if is-test? @tests [])}))

(defn collect [root]
  (let [files (pruned-file-seq root)
        py-files (->> files
                      (map #(.getPath ^java.io.File %))
                      (filter #(python-projection/src-exts (fs/extension %))))
        py-projections (when (seq py-files)
                         (python-projection/collect-files py-files))
        out (atom {:vars [] :tests [] :ns-set #{} :ns→aliases {} :ns→file {}})]
    (doseq [f files]
      (let [path (.getPath ^java.io.File f)
            ext (fs/extension path)]
        (cond
          (src-exts ext)
          (let [forms (read-forms f)
                nf (ns-form forms)
                nsym (when nf (second nf))]
            (when nsym
              (swap! out update :ns-set conj (str nsym))
              (let [is-test? (or (str/includes? path "/test/")
                                 (str/includes? path "/tests/")
                                 (str/ends-with? path "_test.clj")
                                 (str/ends-with? path "_test.cljs")
                                 (str/ends-with? path "_test.cljc")
                                 (str/ends-with? path ".spec.ts"))
                    aliases (parse-requires nf)
                    {:keys [vars tests]} (extract-defs nsym is-test? forms)]
                (swap! out update :vars into vars)
                (swap! out update :tests into tests)
                (swap! out assoc-in [:ns→aliases (str nsym)] aliases)
                (swap! out assoc-in [:ns→file (str nsym)] path))))

          (elisp-projection/src-exts ext)
          (let [{:keys [ns aliases vars tests]} (elisp-projection/collect-file path)]
            (swap! out update :ns-set conj ns)
            (swap! out update :vars into vars)
            (swap! out update :tests into tests)
            (swap! out assoc-in [:ns→aliases ns] aliases)
            (swap! out assoc-in [:ns→file ns] path))

          (python-projection/src-exts ext)
          (let [{:keys [ns aliases vars tests]} (get py-projections path)]
            (swap! out update :ns-set conj ns)
            (swap! out update :vars into vars)
            (swap! out update :tests into tests)
            (swap! out assoc-in [:ns→aliases ns] aliases)
            (swap! out assoc-in [:ns→file ns] path))

          (flexiarg-projection/src-exts ext)
          (when-let [{:keys [ns aliases vars tests]} (flexiarg-projection/collect-file path)]
            (swap! out update :ns-set conj ns)
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

(defn resolve-symbol [s cur-ns aliases by-ns]
  (let [nm (name s) nsp (namespace s)]
    (cond
      (and (nil? nsp) (get-in by-ns [cur-ns nm])) (get-in by-ns [cur-ns nm])
      (and (nil? nsp) aliases)
      (let [a-sym (symbol nm)
            target-ns (some-> (get aliases a-sym) str)]
        (when (and target-ns (get-in by-ns [target-ns nm]))
          (get-in by-ns [target-ns nm])))
      (nil? nsp)
      (when-let [target-ns (prefixed-elisp-ns nm (keys by-ns))]
        (get-in by-ns [target-ns nm]))
      (and nsp aliases)
      (let [a-sym (symbol nsp)
            target-ns (some-> (get aliases a-sym) str)]
        (when (and target-ns (get-in by-ns [target-ns nm]))
          (get-in by-ns [target-ns nm])))
      (and nsp (get-in by-ns [nsp nm])) (get-in by-ns [nsp nm]))))

(defn build-edges [{:keys [vars tests ns→aliases]}]
  (let [by-ns (reduce (fn [acc v]
                        (assoc-in acc [(:var/ns v) (:var/name v)] (:var/qname v)))
                      {} vars)
        cov (atom []) cal (atom [])]
    (doseq [t tests s (:test/syms t)
            :let [aliases (get ns→aliases (:test/ns t))
                  qn (resolve-symbol s (:test/ns t) aliases by-ns)]
            :when (and qn (not= qn (:test/qname t)))]
      (swap! cov conj {:src (:test/qname t) :dst qn}))
    (doseq [v vars s (:var/syms v)
            :let [aliases (get ns→aliases (:var/ns v))
                  qn (resolve-symbol s (:var/ns v) aliases by-ns)]
            :when (and qn (not= qn (:var/qname v)))]
      (swap! cal conj {:src (:var/qname v) :dst qn}))
    {:coverage (vec (distinct @cov)) :calls (vec (distinct @cal))}))

(defn clean-term [s]
  (-> s (str/replace #"\s*\([^)]*\)\s*" "")
        (str/replace #"^[\s*#\-•]+" "")
        (str/replace #"\s+" " ") str/trim str/lower-case))

(defn good-term? [t]
  (and (string? t)
       (let [n (count t)] (and (>= n 3) (<= n 40)))
       (re-matches #"[a-z][a-z0-9 \-/&]*" t)
       (not (vocab-stopwords t))
       (not (re-matches #"^[0-9]+$" t))
       (not (re-find #"^(apply|check|combine|bias|blend|audit|update|select|compute|emit|find|use|get|put|drop|skip)\b" t))
       (<= (count (str/split t #"\s+")) 4)))

(defn extract-vocab-terms [path]
  (let [text (slurp path)
        from-h (->> (str/split-lines text)
                    (keep #(when-let [m (re-find #"^#{2,}\s+(.+?)\s*$" %)]
                             (clean-term (second m)))))
        from-b (map (comp clean-term second) (re-seq #"\*\*([^*\n]+?)\*\*" text))
        from-i (map (comp clean-term second) (re-seq #"(?<!\*)\*([^*\n]{2,40})\*(?!\*)" text))]
    (into (sorted-set) (filter good-term? (concat from-h from-b from-i)))))

(defn term-pattern [terms]
  (when (seq terms)
    (let [escape (fn [t]
                   (-> t (str/replace #"([.\\+*?\[\]^$|(){}])" "\\\\$1")
                       (str/replace #"\s+" "\\\\s+")))
          alt (str/join "|" (map escape (sort-by (comp - count) terms)))]
      (re-pattern (str "(?i)\\b(" alt ")\\b")))))

(defn- normalise-term-hit
  "The term-pattern regex allows `\\s+` between tokens of multi-word terms,
   so a captured group can contain runs of whitespace (newlines, tabs,
   double-spaces) that aren't in the canonical term-set. Collapse them
   back to single spaces to match `extract-vocab-terms`'s output."
  [s]
  (-> s str/lower-case (str/replace #"\s+" " ") str/trim))

(defn scan-file-for-terms [path rx]
  (when rx
    (let [text (slurp path) hits (re-seq rx text)]
      (into #{} (map (comp normalise-term-hit
                           #(if (vector? %) (second %) %)) hits)))))

;; ---------- futon1a write surface ----------

(defn post-hyperedge!
  "POST one hyperedge to futon1a. Returns {:ok? bool :status int :body any}.
   L0 invariant: returns {:ok? true} only when HTTP 200 with body containing :hyperedge.
   `labels` is a vector of tag strings (substrate-1 convention).
   `props` is a map; substrate-1 upserts REPLACE props on re-POST.
   For directed edge types, appends a direction marker so the stable-ID
   distinguishes A→B from B→A."
  [hx-type endpoints labels & [props]]
  (let [endpoints (directed-endpoints hx-type endpoints)
        payload (cond-> {"hx/type" hx-type
                         "hx/endpoints" endpoints}
                  (seq labels) (assoc "hx/labels" labels)
                  props (assoc "hx/props" props))
        resp (try
               (http/post (str FUTON1A "/api/alpha/hyperedge")
                          {:headers {"Content-Type" "application/json"
                                     "X-Penholder" PENHOLDER}
                           :body (json/generate-string payload)
                           :throw false})
               (catch Exception e
                 {:status -1 :body (.getMessage e)}))
        body (when (string? (:body resp))
               (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp))))]
    {:ok? (and (= 200 (:status resp))
               (or (:hyperedge body) (:hx/id body)))
     :status (:status resp)
     :body body}))

;; ---------- inside-out invariant guards ----------

(defn validate-payload!
  "L4: payload shape check before write."
  [hx-type endpoints]
  (when-not (string? hx-type)
    (throw (ex-info "L4 invalid hx-type" {:error/layer 4 :error/status 400
                                          :error/reason :invalid-type :error/context {:type hx-type}})))
  (when-not (and (vector? endpoints) (every? string? endpoints))
    (throw (ex-info "L4 invalid endpoints" {:error/layer 4 :error/status 400
                                            :error/reason :invalid-endpoints :error/context {:endpoints endpoints}}))))

(defn check-endpoint-resolution!
  "L2: every NON-SYNTHETIC endpoint must exist in the vertex-set we built.
   Synthetic direction-marker endpoints (`dir:src→dst`) are skipped because
   they are added later by `directed-endpoints` for stable-ID disambiguation."
  [edge-type endpoints vertex-set]
  (let [real (remove #(str/starts-with? % "dir:") endpoints)]
    (when-not (every? vertex-set real)
      (let [missing (vec (remove vertex-set real))]
        (throw (ex-info (str "L2 endpoint not in vertex set for " edge-type)
                        {:error/layer 2 :error/status 500
                         :error/reason :unresolved-endpoint
                         :error/context {:edge-type edge-type :missing missing}}))))))

;; ---------- main ----------

(defn parse-args [argv]
  (loop [a argv opts {:vocab [] :label nil}]
    (cond
      (empty? a) opts
      (= "--vocab" (first a)) (recur (drop 2 a) (update opts :vocab conj (second a)))
      (= "--label" (first a)) (recur (drop 2 a) (assoc opts :label (second a)))
      (str/starts-with? (first a) "--") (recur (rest a) opts)
      :else (recur (rest a) (assoc opts :root (first a))))))

(defn -main [& argv]
  (let [{:keys [root vocab label]} (parse-args argv)
        repo-label (or label (fs/file-name root))
        labels ["v05" "phase-1" repo-label]
        base-props {"repo" repo-label "phase" 1}
        col (collect root)
        {:keys [coverage calls]} (build-edges col)
        vocab-terms (when (seq vocab)
                      (apply clojure.set/union
                             (map extract-vocab-terms vocab)))
        rx (term-pattern vocab-terms)
        ns→terms (when rx
                   (into {} (for [[ns path] (:ns→file col)]
                              [ns (scan-file-for-terms path rx)])))
        vocab-uses (when ns→terms
                     (for [[ns terms] ns→terms t terms]
                       {:src ns :dst t}))
        vertex-qnames (set (concat (map :var/qname (:vars col))
                                   (map :test/qname (:tests col))
                                   (:ns-set col)))
        term-set (set (or vocab-terms #{}))
        stats (atom {:written 0 :failed 0 :by-type {}})
        bump! (fn [t ok?]
                (swap! stats (fn [s]
                               (-> s
                                   (update (if ok? :written :failed) inc)
                                   (update-in [:by-type t] (fnil inc 0))))))]
    (println "[ingest_v05_to_futon1a] root=" root " label=" repo-label)
    (println "  vars=" (count (:vars col))
             "tests=" (count (:tests col))
             "ns=" (count (:ns-set col))
             "terms=" (count term-set))
    (println "  coverage-edges=" (count coverage)
             "call-edges=" (count calls)
             "vocab-uses=" (count (or vocab-uses [])))
    (println)
    ;; B-2 v0: per-repo prefix on per-repo qnames so cross-codebase qname
    ;; collisions don't overwrite each other's :repo prop. Global types
    ;; (term, doc, term-defines, author, commit) are NOT prefixed.
    (let [pf (fn [q] (str repo-label "/" q))   ; per-repo prefix
          post! (fn [t eps & [extra-props]]
                  (post-hyperedge! t eps labels (merge base-props extra-props)))]
      ;; vertices first
      (println "[L4→L0] writing vertices (per-repo prefixed)")
      (doseq [v (:vars col)]
        (let [t "code/v05/var" eps [(pf (:var/qname v))]]
          (validate-payload! t eps)
          (bump! t (:ok? (post! t eps {"var/ns" (:var/ns v)
                                       "var/qname" (:var/qname v)
                                       "var/kind" (:var/kind v)
                                       "var/has-doc" (:var/has-doc v)})))))
      (doseq [t-vert (:tests col)]
        (let [t "code/v05/test" eps [(pf (:test/qname t-vert))]]
          (validate-payload! t eps)
          (bump! t (:ok? (post! t eps {"test/ns" (:test/ns t-vert)
                                       "test/qname" (:test/qname t-vert)})))))
      (doseq [n (:ns-set col)]
        (let [t "code/v05/namespace" eps [(pf n)]]
          (validate-payload! t eps)
          (bump! t (:ok? (post! t eps {"namespace" n})))))
      (doseq [tm term-set]
        (let [t "code/v05/term" eps [tm]]   ; global; no prefix
          (validate-payload! t eps)
          (bump! t (:ok? (post! t eps)))))
      (doseq [doc-path vocab]
        (let [t "code/v05/doc" eps [doc-path]]   ; global; no prefix
          (validate-payload! t eps)
          (bump! t (:ok? (post! t eps)))))
      ;; edges (with L2 endpoint check; L2 sees prefixed qnames now)
      (println "[L4→L0] writing edges (per-repo prefixed)")
      (let [prefixed-vertex-qs (set (map pf vertex-qnames))]
        (doseq [e calls]
          (let [t "code/v05/calls" eps [(pf (:src e)) (pf (:dst e))]]
            (validate-payload! t eps)
            (check-endpoint-resolution! t eps prefixed-vertex-qs)
            (bump! t (:ok? (post! t eps)))))
        (doseq [e coverage]
          (let [t "code/v05/coverage" eps [(pf (:src e)) (pf (:dst e))]]
            (validate-payload! t eps)
            (check-endpoint-resolution! t eps prefixed-vertex-qs)
            (bump! t (:ok? (post! t eps))))))
      (doseq [e (or vocab-uses [])]
        ;; vocab-use: src ns is per-repo (prefix), dst term is global (no prefix)
        (let [t "code/v05/vocabulary-use" eps [(pf (:src e)) (:dst e)]]
          (validate-payload! t eps)
          (when-not ((:ns-set col) (:src e))
            (throw (ex-info "L2 vocab-use src not a namespace"
                            {:error/layer 2 :error/reason :unresolved-source :error/context e})))
          (when-not (term-set (:dst e))
            (throw (ex-info "L2 vocab-use dst not a term"
                            {:error/layer 2 :error/reason :unresolved-target :error/context e})))
          (bump! t (:ok? (post! t eps)))))
      (doseq [doc-path vocab tm (extract-vocab-terms doc-path)]
        ;; term-defines: global, no prefix
        (let [t "code/v05/term-defines" eps [doc-path tm]]
          (validate-payload! t eps)
          (when (term-set tm)
            (bump! t (:ok? (post! t eps))))))
      (doseq [[ns _] (:ns→file col)
              v (:vars col) :when (= (:var/ns v) ns)]
        ;; contains: per-repo on both endpoints
        (let [t "code/v05/contains" eps [(pf ns) (pf (:var/qname v))]]
          (validate-payload! t eps)
          (bump! t (:ok? (post! t eps))))))
    (println)
    (println "=== ingest summary ===")
    (println " label:" repo-label)
    (println " written:" (:written @stats) " failed:" (:failed @stats))
    (doseq [[t n] (sort-by key (:by-type @stats))]
      (println (format "  %-30s %d" t n)))
    (when (pos? (:failed @stats))
      (println "WARN: some writes failed; see stderr for details")
      (System/exit 1))))

(apply -main *command-line-args*)
