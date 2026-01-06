(ns scripts.suggest-links
  "Suggest document and code links based on devmap references and sexp scans."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.hx.api :as hx]
            [futon3.hx.store :as store]))

(def devmap-dir "/home/joe/code/futon3/holes")
(def repo-root "/home/joe/code")
(def futon1-api-base (or (System/getenv "FUTON1_API_BASE")
                         "http://localhost:8080/api/alpha"))
(def clj-dirs ["/home/joe/code/futon3/src/f2"
               "/home/joe/code/futon3/src/futon3"])
(def elisp-dirs ["/home/joe/code/futon3/contrib"
                 "/home/joe/code/futon3/scripts"
                 "/home/joe/code/futon3/holes"])
(def docbook-books ["futon0" "futon1" "futon2" "futon3" "futon4"])
(def org-files ["/home/joe/code/futon3/holes/orpm.org"
                "/home/joe/code/futon3/holes/aob.org"])

(defn- file-ext [^String path]
  (when-let [idx (str/last-index-of path ".")]
    (subs path (inc idx))))

(defn- artifact-type [path]
  (case (file-ext path)
    "clj" :clojure
    "cljc" :clojure
    "el" :elisp
    "md" :markdown
    "org" :org
    "devmap" :devmap
    :file))

(defn- normalize-path [token]
  (cond
    (str/starts-with? token "/home/joe/code/") token
    (re-find #"^futon[0-9]+/" token) (str repo-root "/" token)
    :else nil))

(defn- extract-paths [line]
  (->> (re-seq #"(/home/joe/code/[^\s\)\]\}\"']+|futon[0-9]+/[^\s\)\]\}\"']+)" line)
       (map first)
       (map normalize-path)
       (remove nil?)
       distinct))

(defn- devmap-files []
  (->> (file-seq (io/file devmap-dir))
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (str %) ".devmap"))))

(defn- docbook-target-from-path [path]
  (when-let [m (re-find #"/futon4/docs/docbook/(futon[0-9]+)/([^/]+)\\.org$" path)]
    {:book (nth m 1)
     :doc-id (nth m 2)}))

(defn- existing-org-files []
  (->> org-files
       (map io/file)
       (filter #(.exists ^java.io.File %))))

(defn- clj-files []
  (->> clj-dirs
       (mapcat #(file-seq (io/file %)))
       (filter #(.isFile ^java.io.File %))
       (filter (fn [f]
                 (let [path (str f)]
                   (or (str/ends-with? path ".clj")
                       (str/ends-with? path ".cljc")))))))

(defn- elisp-files []
  (->> elisp-dirs
       (mapcat #(file-seq (io/file %)))
       (filter #(.isFile ^java.io.File %))
       (filter (fn [f]
                 (str/ends-with? (str f) ".el")))))

(defn- slugify [s]
  (-> s
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"(^-|-$)" "")))

(defn- title-from-line [line]
  (when-let [m (re-find #"^! instantiated-by:\s*(.+)$" line)]
    (second m)))

(defn- ensure-artifact! [path]
  (when (.exists (io/file path))
    (hx/register-artifact! {:artifact/id path
                            :artifact/type (artifact-type path)
                            :artifact/title (str/replace-first path (str repo-root "/") "")
                            :artifact/path path})))

(defn- fetch-pattern-registry []
  (try
    (let [url (str futon1-api-base "/patterns/registry")
          body (slurp url)
          data (json/parse-string body true)
          registry (or (:registry data) {})]
      (:entities registry))
    (catch Exception _
      nil)))

(defn- fetch-docbook-toc [book]
  (try
    (let [url (str futon1-api-base "/docs/" book "/toc")
          body (slurp url)
          data (json/parse-string body true)]
      (:headings data))
    (catch Exception _ nil)))

(defn- build-pattern-index []
  (when-let [entities (fetch-pattern-registry)]
    (let [by-name (group-by (comp str/lower-case :name) entities)
          by-external (group-by :external-id entities)]
      {:entities entities
       :by-name by-name
       :by-external by-external})))

(defn- build-docbook-index []
  (let [headings (->> docbook-books
                      (mapcat (fn [book]
                                (when-let [items (fetch-docbook-toc book)]
                                  (map #(assoc % :doc/book book) items))))
                      (remove nil?))
        by-id (group-by :doc/id headings)]
    {:headings headings
     :by-id by-id}))

(defn- ensure-pattern-artifact! [pattern]
  (let [pid (:id pattern)
        name (:name pattern)
        artifact-id (str "futon1/pattern/" pid)]
    (hx/register-artifact! {:artifact/id artifact-id
                            :artifact/type :pattern
                            :artifact/title name
                            :artifact/path artifact-id})
    {:artifact/id artifact-id}))

(defn- ensure-docbook-artifact! [book doc-id]
  (let [artifact-id (str "futon1/docbook/" book "/" doc-id)]
    (hx/register-artifact! {:artifact/id artifact-id
                            :artifact/type :docbook
                            :artifact/title doc-id
                            :artifact/path artifact-id})
    {:artifact/id artifact-id}))

(defn- read-forms [^java.io.Reader rdr]
  (binding [*read-eval* false]
    (loop [forms []]
      (let [form (try
                   (read rdr false ::eof)
                   (catch Exception _ ::eof))]
        (if (= form ::eof)
          forms
          (recur (conj forms form)))))))

(defn- collect-cars [form]
  (let [cars (atom [])]
    (letfn [(walk [node]
              (cond
                (seq? node)
                (do
                  (when-let [head (first node)]
                    (when (symbol? head)
                      (swap! cars conj (name head))))
                  (doseq [child node] (walk child)))

                (vector? node)
                (doseq [child node] (walk child))

                (map? node)
                (doseq [child (concat (keys node) (vals node))]
                  (walk child))

                :else nil))]
      (walk form)
      @cars)))

(defn- cars-from-file [file]
  (try
    (with-open [rdr (java.io.PushbackReader. (io/reader file))]
      (->> (read-forms rdr)
           (mapcat collect-cars)
           distinct))
    (catch Exception _ [])))

(defn- build-anchor-index []
  (let [state (store/state)]
    (->> (:anchors state)
         vals
         (mapcat identity)
         (keep (fn [anchor]
                 (when (#{:defn :defmulti :defun} (:anchor/kind anchor))
                   [(str (:anchor/title anchor))
                    {:artifact/id (:anchor/artifact anchor)
                     :anchor/id (:anchor/id anchor)}])))
         (group-by first)
         (reduce-kv (fn [acc k v]
                      (assoc acc k (map second v)))
                    {}))))

(defn- collect-pattern-ids [form]
  (let [ids (atom [])]
    (letfn [(walk [node]
              (cond
                (map? node)
                (do
                  (when-let [pid (get node :pattern/id)]
                    (cond
                      (keyword? pid) (swap! ids conj (subs (str pid) 1))
                      (string? pid) (swap! ids conj pid)
                      :else nil))
                  (doseq [child (concat (keys node) (vals node))]
                    (walk child)))

                (seq? node)
                (doseq [child node] (walk child))

                (vector? node)
                (doseq [child node] (walk child))

                :else nil))]
      (walk form)
      (distinct @ids))))

(defn- resolve-pattern-targets [pattern-index pattern-id]
  (if-not pattern-index
    []
    (let [by-external (get-in pattern-index [:by-external pattern-id])
          by-name (get-in pattern-index [:by-name (str/lower-case pattern-id)])
          matches (or by-external by-name)]
      (vec (for [pattern matches]
             (ensure-pattern-artifact! pattern))))))

(defn- normalize-pattern-id [value]
  (cond
    (keyword? value) (subs (str value) 1)
    (string? value) (str/trim value)
    :else nil))

(defn- pattern-ids-from-org [lines]
  (->> lines
       (mapcat (fn [line]
                 (when-let [m (re-find #":PATTERN:\s*([^\s]+)" line)]
                   [(second m)])))
       (map normalize-pattern-id)
       (remove nil?)
       distinct))

(defn -main [& _]
  (store/load-log!)
  (let [files (devmap-files)
        anchor-index (build-anchor-index)
        pattern-index (build-pattern-index)
        docbook-index (build-docbook-index)
        devmap-results
        (for [file files
              :let [lines (line-seq (io/reader file))
                    devmap-path (str file)
                    _ (hx/register-artifact! {:artifact/id devmap-path
                                              :artifact/type :devmap
                                              :artifact/title (.getName ^java.io.File file)
                                              :artifact/path devmap-path})]]
          (loop [remaining lines
                 count-links 0]
            (if (empty? remaining)
              {:file devmap-path :links count-links}
              (let [line (first remaining)
                    title (title-from-line line)
                    paths (extract-paths line)
                    link-results (for [path paths
                                       :let [docbook-target (docbook-target-from-path path)
                                             link-target (cond
                                                           (and docbook-target docbook-index)
                                                           (let [book (:book docbook-target)
                                                                 doc-id (:doc-id docbook-target)
                                                                 matches (get-in docbook-index [:by-id doc-id])]
                                                             (when (seq matches)
                                                               (ensure-docbook-artifact! book doc-id)))
                                                           :else
                                                           (do (ensure-artifact! path)
                                                               {:artifact/id path}))
                                             link {:link/from {:artifact/id devmap-path}
                                                   :link/to link-target
                                                   :link/type :documents
                                                   :link/agent :devmap-ingest
                                                   :link/rationale "devmap reference"}]]
                                   (when link-target (hx/suggest-link! link)))
                    pattern-results (when (and title pattern-index)
                                      (let [matches (get-in pattern-index [:by-name (str/lower-case title)])]
                                        (for [pattern matches
                                              :let [target (ensure-pattern-artifact! pattern)
                                                    link {:link/from {:artifact/id devmap-path}
                                                          :link/to target
                                                          :link/type :applies-pattern
                                                          :link/agent :devmap-ingest
                                                          :link/rationale "devmap title matched futon1 registry"}]]
                                          (hx/suggest-link! link))))
                    link-count (+ count-links (count (remove nil? link-results)) (count pattern-results))]
                (recur (rest remaining) link-count)))))
        org-results
        (for [file (existing-org-files)
              :let [lines (line-seq (io/reader file))
                    path (str file)
                    _ (ensure-artifact! path)
                    ids (pattern-ids-from-org lines)
                    resolved (map (fn [pid] [pid (resolve-pattern-targets pattern-index pid)]) ids)
                    targets (distinct (mapcat second resolved))
                    missing-ids (map first (filter (fn [[_ targets]] (empty? targets)) resolved))
                    link-results (for [target targets
                                       :let [link {:link/from {:artifact/id path}
                                                   :link/to target
                                                   :link/type :applies-pattern
                                                   :link/agent :org-ingest
                                                   :link/rationale "org :PATTERN: property"}]]
                                   (hx/suggest-link! link))]]
          {:file path
           :links (count link-results)
           :pattern-ids (count ids)
           :missing (count missing-ids)
           :missing-ids (vec missing-ids)})
        code-files (concat (clj-files) (elisp-files))
        code-results
        (for [file code-files
              :let [path (str file)
                    _ (ensure-artifact! path)
                    forms (try
                            (with-open [rdr (java.io.PushbackReader. (io/reader file))]
                              (read-forms rdr))
                            (catch Exception _ []))
                    cars (distinct (mapcat collect-cars forms))
                    targets (distinct (mapcat #(get anchor-index % []) cars))
                    pattern-ids (distinct (mapcat collect-pattern-ids forms))
                    resolved-patterns (map (fn [pid] [pid (resolve-pattern-targets pattern-index pid)]) pattern-ids)
                    pattern-targets (distinct (mapcat second resolved-patterns))
                    missing-patterns (count (filter (fn [[_ targets]] (empty? targets)) resolved-patterns))
                    uses-results (for [target targets
                                       :when (not= path (:artifact/id target))
                                       :let [link {:link/from {:artifact/id path}
                                                   :link/to target
                                                   :link/type :uses
                                                   :link/agent :static-scan
                                                   :link/rationale "sexp car reference"}]]
                                   (hx/suggest-link! link))
                    applies-results (for [target pattern-targets
                                          :let [link {:link/from {:artifact/id path}
                                                      :link/to target
                                                      :link/type :applies-pattern
                                                      :link/agent :static-scan
                                                      :link/rationale "pattern/id reference"}]]
                                      (hx/suggest-link! link))
                    link-count (+ (count uses-results) (count applies-results))]]
          {:file path
           :links link-count
           :pattern-ids (count pattern-ids)
           :pattern-links (count applies-results)
           :pattern-missing missing-patterns})]
    (println (pr-str {:devmaps (count devmap-results)
                      :org-files (count org-results)
                      :code-files (count code-results)
                      :docbook-headings (count (:headings docbook-index))
                      :links (+ (reduce + 0 (map :links devmap-results))
                                (reduce + 0 (map :links org-results))
                                (reduce + 0 (map :links code-results)))
                      :devmap-results devmap-results
                      :org-results org-results
                      :code-results code-results}))))

(when (= *file* (System/getProperty "babashka.file"))
  (-main))
