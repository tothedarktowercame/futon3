(ns scripts.pattern-sync
  "Push-first sync: filesystem flexiarg libraries -> Futon1/XTDB."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.client :as http])
  (:import (java.io File)
           (java.net URLEncoder)))

(def ^:private default-ego-limit 128)
(def ^:private default-timeout-ms 120000)
(def ^:private client-schema "0.1.0")

(def ^:private language-relation ":pattern-language/includes")
(def ^:private language-source-relation ":language/source")
(def ^:private language-status-relation ":language/status")
(def ^:private language-catalog-name "pattern-language/catalog")
(def ^:private language-catalog-relation ":language/catalog")
(def ^:private default-ad-hoc-status "pattern-language/status/draft")
(def ^:private default-canonical-status "pattern-language/status/published")

(defn- usage []
  (str/join
   "\n"
   ["Usage: clj -M -m scripts.pattern-sync [--root PATH] [--library PATH] [--dir PATH]"
    "                                   [--api-base URL] [--profile NAME] [--dry-run] [--diff] [--registry]"
    "                                   [--summary-only] [--libraries-only] [--skip-model] [--skip-verify]"
    "                                   [--timeout-ms N]"
    "Environment variables:"
    "  FUTON1_API_BASE   Base URL for Futon1 API (e.g. http://localhost:8080/api/alpha)"
    "  FUTON1_PROFILE    Optional profile header"
    "  FUTON3_ROOT       Default root if --root not provided"]))

(defn- parse-args [args]
  (loop [opts {:root (or (some-> (System/getenv "FUTON3_ROOT") str/trim not-empty) ".")
               :library nil
               :dirs []
               :dry-run? false
               :diff? false
               :registry? false
               :summary-only? false
               :libraries-only? false
               :skip-model? false
               :skip-verify? false
               :timeout-ms nil
               :api-base nil
               :profile nil
               :plan (atom [])
               :registry nil
               :registry-index nil}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--root" (recur (assoc opts :root (second remaining)) (nnext remaining))
        "--library" (recur (assoc opts :library (second remaining)) (nnext remaining))
        "--dir" (recur (update opts :dirs conj (second remaining)) (nnext remaining))
        "--api-base" (recur (assoc opts :api-base (second remaining)) (nnext remaining))
        "--profile" (recur (assoc opts :profile (second remaining)) (nnext remaining))
        "--dry-run" (recur (assoc opts :dry-run? true) (rest remaining))
        "--diff" (recur (assoc opts :diff? true) (rest remaining))
        "--registry" (recur (assoc opts :registry? true) (rest remaining))
        "--summary-only" (recur (assoc opts :summary-only? true) (rest remaining))
        "--libraries-only" (recur (assoc opts :libraries-only? true) (rest remaining))
        "--skip-model" (recur (assoc opts :skip-model? true) (rest remaining))
        "--skip-verify" (recur (assoc opts :skip-verify? true) (rest remaining))
        "--timeout-ms" (recur (assoc opts :timeout-ms (Long/parseLong (second remaining))) (nnext remaining))
        "-h" (recur (assoc opts :help? true) (rest remaining))
        "--help" (recur (assoc opts :help? true) (rest remaining))
        (throw (ex-info (str "Unknown argument " arg) {:arg arg})))
      opts)))

(defn- normalize-base [base]
  (some-> base str/trim (str/replace #"/+$" "")))

(defn- api-base [opts]
  (or (normalize-base (:api-base opts))
      (normalize-base (System/getenv "FUTON1_API_BASE"))))

(defn- api-root [opts]
  (let [base (api-base opts)]
    (when base
      (if (re-find #"/api/(alpha|%CE%B1|%ce%b1|%CE%91|%ce%91)$" base)
        base
        (str base "/api/alpha")))))

(defn- profile [opts]
  (or (:profile opts)
      (some-> (System/getenv "FUTON1_PROFILE") str/trim not-empty)
      (some-> (System/getenv "FUTON1_API_PROFILE") str/trim not-empty)))

(defn- headers [opts]
  (cond-> {"content-type" "application/json"}
    (seq (profile opts)) (assoc "x-profile" (profile opts))))

(defn- clean-text [value]
  (when (string? value)
    (let [trimmed (str/trim value)
          lowered (str/lower-case trimmed)]
      (when (and (seq trimmed)
                 (not= lowered "external"))
        trimmed))))

(defn- maybe-source [payload]
  (if-let [src (clean-text (:source payload))]
    (assoc payload :source src)
    (dissoc payload :source)))

(defn- encode-segment [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn- request-json! [opts method path payload]
  (when-not (api-root opts)
    (throw (ex-info "FUTON1_API_BASE is not set" {})))
  (let [url (str (api-root opts) path)
        body (when payload (json/encode payload {:escape-non-ascii true}))
        timeout (or (:timeout-ms opts) default-timeout-ms)
        req (case method
              :get #(http/get url {:headers (headers opts)
                                   :timeout timeout
                                   :idle-timeout timeout})
              :post #(http/post url {:headers (headers opts)
                                     :timeout timeout
                                     :idle-timeout timeout
                                     :body body}))
        {:keys [status error body]} @(req)]
    (when error
      (throw (ex-info "HTTP error" {:url url :error error :timeout timeout})))
    {:status status
     :body (when body (json/parse-string body true))}))

(defn- parse-version [value]
  (->> (re-seq #"\d+" (str value))
       (mapv #(Long/parseLong %))))

(defn- compare-versions [a b]
  (let [av (parse-version a)
        bv (parse-version b)
        max-len (max (count av) (count bv))
        pad (fn [v] (vec (concat v (repeat (- max-len (count v)) 0))))]
    (compare (pad av) (pad bv))))

(defn- version>=? [a b]
  (not (neg? (compare-versions a b))))

(defn- report? [opts]
  (or (:dry-run? opts) (:diff? opts)))

(defn- write-enabled? [opts]
  (not (report? opts)))

(defn- plan! [opts entry]
  (when-let [plan (:plan opts)]
    (swap! plan conj entry)))

(declare relation-text)

(defn- relation-key [value]
  (relation-text value))

(declare slugify)

(defn- tail-slug [value]
  (when (and value (not (str/blank? (str value))))
    (->> (str/split (str value) #"/")
         last
         slugify)))

(defn- build-registry-index [registry]
  (when registry
    (let [entities (or (:entities registry) [])
          relations (or (:relations registry) [])]
      {:entities-by-name
       (into {}
             (map (fn [entity]
                    [(:name entity) entity]))
             entities)
       :entities-by-external-id
       (->> entities
            (filter (fn [entity]
                      (let [external (:external-id entity)]
                        (and external (not (str/blank? (str external)))))))
            (into {}
                  (map (fn [entity]
                         [(:external-id entity) entity]))))
       :entities-by-key
       (->> entities
            (mapcat (fn [entity]
                      (let [name (:name entity)
                            external (:external-id entity)]
                        (cond-> []
                          (and name (not (str/blank? (str name))))
                          (conj [name entity])
                          (and external (not (str/blank? (str external))))
                          (conj [external entity])))))
            (into {}))
       :entities-by-slug
       (->> entities
            (keep (fn [entity]
                    (when-let [name (:name entity)]
                      (let [slug (slugify name)
                            tail (tail-slug name)]
                        (cond-> []
                          (and slug (not (str/blank? slug)))
                          (conj [slug entity])
                          (and tail (not (str/blank? tail)))
                          (conj [tail entity]))))))
            (mapcat identity)
            (into {}))
       :relations-by-src
       (reduce (fn [acc rel]
                 (let [src-id (:src-id rel)
                       dst-id (:dst-id rel)
                       rel-type (relation-key rel)]
                   (if (and src-id dst-id rel-type)
                     (update acc [src-id rel-type] (fnil conj #{}) dst-id)
                     acc)))
               {}
               relations)})))

(defn- fetch-registry [opts]
  (let [{:keys [body]} (request-json! opts :get "/patterns/registry" nil)]
    (or (:registry body) body)))

(defn- fetch-model [opts]
  (request-json! opts :get "/meta/model" nil))

(defn- fetch-model-verify [opts]
  (request-json! opts :get "/meta/model/verify" nil))

(defn- maybe-load-registry [opts]
  (if (and (report? opts) (:registry? opts))
    (try
      (let [registry (fetch-registry opts)]
        (assoc opts
               :registry registry
               :registry-index (build-registry-index registry)))
      (catch Exception ex
        (binding [*out* *err*]
          (println (format "Registry fetch failed: %s" (.getMessage ex))))
        opts))
    opts))

(defn- maybe-load-model [opts]
  (if (:skip-model? opts)
    opts
    (let [{:keys [status body]} (fetch-model opts)]
      (if (= 200 status)
        (assoc opts :model body)
        (throw (ex-info "Model descriptor not available"
                        {:status status
                         :body body}))))))

(defn- enforce-model-compat! [opts]
  (when-let [model (:model opts)]
    (let [descriptor (:descriptor model)
          schema-version (:schema/version descriptor)
          min-schema (:client/schema-min descriptor)
          hash (:hash model)]
      (when (and min-schema (not (version>=? client-schema min-schema)))
        (throw (ex-info "Client schema too old for descriptor"
                        {:client-schema client-schema
                         :required min-schema})))
      (println (format "Model descriptor: schema=%s hash=%s client-schema=%s"
                       (or schema-version "?")
                       (or hash "?")
                       client-schema)))))

(defn- enforce-model-verify! [opts]
  (when-not (:skip-verify? opts)
    (let [{:keys [status body]} (fetch-model-verify opts)]
      (when-not (= 200 status)
        (throw (ex-info "Model verify endpoint failed"
                        {:status status
                         :body body})))
      (when-not (:ok? body)
        (throw (ex-info "Model invariants failed"
                        {:results (:results body)})))
      (println "Model invariants: ok"))))

(defn- fetch-entity [opts name]
  (let [path (str "/entity/" (encode-segment name))]
    (request-json! opts :get path nil)))

(defn- ensure-entity! [opts payload]
  (let [name (:name payload)
        external-id (:external-id payload)
        title-slug (some-> external-id slugify)
        name-tail-slug (tail-slug name)
        fallback {:entity {:name name
                           :type (:type payload)
                           :id (str "dry-run:" name)}}]
    (when (write-enabled? opts)
      (println (format "Ensuring entity: %s (%s)" name (:type payload))))
    (if (report? opts)
      (if-let [index (:registry-index opts)]
        (if-let [entry (or (get-in index [:entities-by-key name])
                           (when external-id
                             (get-in index [:entities-by-key external-id]))
                           (when title-slug
                             (get-in index [:entities-by-slug title-slug]))
                           (when name-tail-slug
                             (get-in index [:entities-by-slug name-tail-slug]))
                           (get-in index [:entities-by-slug name])
                           (get-in index [:entities-by-name name])
                           (when external-id
                             (get-in index [:entities-by-external-id external-id])))]
          {:entity {:id (:id entry)
                    :name (:name entry)
                    :type (:type entry)}}
          (do
            (plan! opts {:action :ensure-entity :name name :type (:type payload)})
            fallback))
        (let [{:keys [status body]} (fetch-entity opts name)]
          (if (= 200 status)
            body
            (do
              (plan! opts {:action :ensure-entity :name name :type (:type payload)})
              fallback))))
      (let [start (System/nanoTime)
            {:keys [body]} (request-json! opts :post "/entity" payload)
            elapsed-ms (/ (double (- (System/nanoTime) start)) 1000000.0)]
        (when (write-enabled? opts)
          (println (format "Ensured entity: %s (%.1fms)" name elapsed-ms)))
        body))))

(defn- create-relation! [opts payload]
  (when (write-enabled? opts)
    (println (format "Ensuring relation: %s -> %s (%s)"
                     (get-in payload [:src :name])
                     (get-in payload [:dst :name])
                     (get-in payload [:provenance :note])))
    (let [start (System/nanoTime)
          resp (request-json! opts :post "/relation" payload)
          elapsed-ms (/ (double (- (System/nanoTime) start)) 1000000.0)]
      (println (format "Ensured relation: %s -> %s (%.1fms)"
                       (get-in payload [:src :name])
                       (get-in payload [:dst :name])
                       elapsed-ms))
      resp)))

(defn- ego [opts name limit]
  (let [path (str "/ego/" (encode-segment name))
        query (when limit (str "?limit=" limit))
        {:keys [body]} (request-json! opts :get (str path query) nil)]
    (:ego body)))

(defn- relation-text [value]
  (let [raw (cond
              (keyword? value) (name value)
              (string? value) value
              (map? value) (or (:label value)
                               (:type value)
                               (:relation/type value))
              :else nil)]
    (when raw
      (if (str/starts-with? raw ":")
        (subs raw 1)
        raw))))

(defn- relation-match? [value target]
  (let [lhs (relation-text value)
        rhs (relation-text target)]
    (and lhs rhs (= lhs rhs))))

(defn- link-entity-id [link]
  (let [entity (:entity link)]
    (or (:id entity)
        (:entity/id entity)
        (:ident entity)
        (:entity/ident entity))))

(defn- outgoing-links [ego]
  (or (:outgoing ego)
      (get-in ego [:links :outgoing])
      []))

(defn- existing-target-ids [opts name relation-label]
  (if-let [src-id (get-in opts [:registry-index :entities-by-name name :id])]
    (let [rel-key (relation-key relation-label)]
      (get-in opts [:registry-index :relations-by-src [src-id rel-key]] #{}))
    (try
      (let [outgoing (outgoing-links (ego opts name default-ego-limit))]
        (->> outgoing
             (filter #(relation-match? (:relation %) relation-label))
             (keep link-entity-id)
             set))
      (catch Exception _
        #{}))))

(defn- slugify [text]
  (let [lower (str/lower-case (or text "component"))
        clean (str/replace lower #"[^a-z0-9]+" "-")
        trimmed (str/replace clean #"(^-+|-+$)" "")]
    (if (str/blank? trimmed) "component" trimmed)))

(defn- trim-empty-lines [lines]
  (let [trimmed-start (drop-while #(re-matches #"\s*" %) lines)
        trimmed-end (reverse (drop-while #(re-matches #"\s*" %) (reverse trimmed-start)))]
    trimmed-end))

(defn- parse-section [label lines]
  (let [clean (trim-empty-lines (reverse lines))
        text (str/trimr (str/join "\n" clean))]
    {:label label
     :slug (slugify label)
     :text text}))

(defn- extract-meta [text key]
  (some->> (re-find (re-pattern (str "@" key "\\s+(.*)")) text)
           second
           str/trim
           not-empty))

(defn- derive-name-from-path [^File file]
  (let [path (.getPath file)
        stripped (str/replace path #"\.[^.]+$" "")
        relative (second (re-find #"/library/(.*)$" stripped))
        base (.getName (io/file stripped))
        dir (.getParent (io/file stripped))
        parent (when dir (.getName (io/file dir)))]
    (or relative
        (when (and parent (not (str/blank? parent)))
          (str parent "/" base))
        base)))

(defn- split-arg-blocks [text]
  (let [lines (str/split-lines text)]
    (loop [remaining lines
           current []
           has-arg? false
           blocks []]
      (if-let [line (first remaining)]
        (let [rest-lines (rest remaining)
              starts-arg? (str/starts-with? line "@arg ")]
          (cond
            (and starts-arg? has-arg?)
            (recur rest-lines [line] true (conj blocks (str/join "\n" current)))

            starts-arg?
            (recur rest-lines (conj current line) true blocks)

            :else
            (recur rest-lines (conj current line) has-arg? blocks)))
        (if has-arg?
          (conj blocks (str/join "\n" current))
          [text])))))

(defn- parse-components [block]
  (let [lines (str/split-lines block)
        header-re (re-pattern "^\\s*[!+]\\s+([^:]+):\\s*(.*)$")]
    (loop [remaining lines
           current nil
           sections []]
      (if-let [line (first remaining)]
        (if-let [[_ label trailing] (re-matches header-re line)]
          (let [next-section (when current
                               (parse-section (:label current) (:lines current)))
                new-lines (cond-> []
                            (and trailing (not (str/blank? trailing)))
                            (conj trailing))]
            (recur (rest remaining)
                   {:label (str/trim label) :lines new-lines}
                   (cond-> sections next-section (conj next-section))))
          (recur (rest remaining)
                 (if current
                   (update current :lines conj line)
                   current)
                 sections))
        (let [final-section (when current
                              (parse-section (:label current) (:lines current)))]
          (cond-> sections final-section (conj final-section)))))))

(defn- parse-block [^File file block]
  (let [title (extract-meta block "title")
        arg (extract-meta block "arg")
        flexiarg (extract-meta block "flexiarg")
        multiarg (extract-meta block "multiarg")
        name (or arg flexiarg multiarg (extract-meta block "name") (derive-name-from-path file))
        components (parse-components block)
        summary (:text (first components))]
    {:name name
     :title title
     :summary (or summary "")
     :components components
     :file file}))

(defn- pattern-file? [^File file]
  (let [name (.getName file)]
    (or (str/ends-with? name ".flexiarg")
        (str/ends-with? name ".multiarg"))))

(defn- pattern-files [^File root]
  (->> (file-seq root)
       (filter #(.isFile ^File %))
       (filter pattern-file?)
       (sort-by #(.lastModified ^File %) >)))

(defn- parse-patterns [^File root]
  (mapcat (fn [file]
            (let [text (slurp file)]
              (map #(parse-block file %) (split-arg-blocks text))))
          (pattern-files root)))

(defn- canonical-directory? [root ^File dir]
  (let [path (.getCanonicalPath dir)
        canonical (io/file root "library")]
    (str/includes? path (str (.getCanonicalPath canonical) File/separator))))

(defn- language-source-name [root dir]
  (if (canonical-directory? root dir)
    "pattern-language/source/futon3-library"
    "pattern-language/source/ad-hoc"))

(defn- language-status-name [root dir explicit]
  (cond
    (and explicit (not (str/blank? explicit))) explicit
    (canonical-directory? root dir) default-canonical-status
    :else default-ad-hoc-status))

(defn- collection-label [^File dir]
  (.getName dir))

(defn- collection-language [^File dir]
  (let [label (collection-label dir)
        slug (or (slugify label) "library")]
    {:name (str "pattern-language/" slug)
     :title label}))

(defn- ensure-relation!
  ([opts src-name src-id relation-label dst-id dst-name]
   (ensure-relation! opts src-name src-id relation-label dst-id dst-name {}))
  ([opts src-name src-id relation-label dst-id dst-name props]
   (let [existing (existing-target-ids opts src-name relation-label)]
     (when-not (contains? existing dst-id)
       (plan! opts {:action :ensure-relation
                    :src src-name
                    :dst dst-id
                    :relation relation-label})
       (create-relation! opts {:type "arxana/scholium"
                               :src {:id src-id
                                     :name src-name}
                               :dst {:id dst-id
                                     :name dst-name}
                               :provenance (merge {:note relation-label} props)})))))

(defn- ensure-tag! [opts language-name language-id relation target-name target-type]
  (let [resp (ensure-entity! opts (maybe-source {:name target-name
                                                 :type target-type
                                                 :source (str relation " classification")
                                                 :external-id target-name}))
        target-id (or (get-in resp [:entity :id]) (get-in resp [:entity :entity/id]))]
    (when target-id
      (ensure-relation! opts language-name language-id relation target-id target-name {}))))

(defn- ensure-catalog-link! [opts language-name language-id]
  (let [resp (ensure-entity! opts (maybe-source {:name language-catalog-name
                                                 :type "pattern/language-catalog"
                                                 :source "Pattern languages"}))
        catalog-id (or (get-in resp [:entity :id]) (get-in resp [:entity :entity/id]))]
    (when catalog-id
      (ensure-relation! opts language-catalog-name catalog-id
                        language-catalog-relation language-id language-name {}))))

(defn- ensure-pattern! [opts pattern]
  (let [{:keys [name title summary components]} pattern
        resp (ensure-entity! opts (maybe-source {:name name
                                                 :type "pattern/library"
                                                 :source summary
                                                 :external-id title}))
        pattern-id (or (get-in resp [:entity :id]) (get-in resp [:entity :entity/id]))
        includes-existing (when name (existing-target-ids opts name ":pattern/includes"))]
    (when-not (:libraries-only? opts)
      (doseq [[idx component] (map-indexed vector components)]
        (let [order (inc idx)
              label (:label component)
              slug (:slug component)
              comp-name (format "%s/%02d-%s" name order slug)
              comp-resp (ensure-entity! opts (maybe-source {:name comp-name
                                                            :type "pattern/component"
                                                            :source (:text component)
                                                            :external-id label}))
              comp-id (or (get-in comp-resp [:entity :id])
                          (get-in comp-resp [:entity :entity/id]))]
          (when (and pattern-id comp-id)
            (when-not (and includes-existing (contains? includes-existing comp-id))
              (create-relation! opts {:type "arxana/scholium"
                                      :src {:id pattern-id
                                            :name name}
                                      :dst {:id comp-id
                                            :name comp-name}
                                      :provenance {:note ":pattern/includes"
                                                   :order order}}))))))
    {:name name
     :id pattern-id}))

(defn- ingest-collection! [opts root dir]
  (let [patterns (vec (parse-patterns dir))
        {:keys [name title]} (collection-language dir)
        language-status (language-status-name root dir nil)
        language-source (language-source-name root dir)
        language-resp (ensure-entity! opts (maybe-source {:name name
                                                          :type "pattern/language"
                                                          :source (format "Imported from %s" (.getPath dir))
                                                          :external-id title}))
        language-id (or (get-in language-resp [:entity :id])
                        (get-in language-resp [:entity :entity/id]))
        existing (when name (existing-target-ids opts name language-relation))]
    (when (and language-id (seq patterns))
      (doseq [[idx pattern] (map-indexed vector patterns)]
        (let [order (inc idx)
              ensured (ensure-pattern! opts pattern)
              pattern-id (:id ensured)]
          (when (and pattern-id (not (contains? existing pattern-id)))
            (create-relation! opts {:type "arxana/scholium"
                                    :src {:id language-id
                                          :name name}
                                    :dst {:id pattern-id
                                          :name (:name ensured)}
                                    :provenance {:note language-relation
                                                 :order order}}))))
      (ensure-tag! opts name language-id language-source-relation
                   language-source "pattern/language-source")
      (ensure-tag! opts name language-id language-status-relation
                   language-status "pattern/language-status")
      (ensure-catalog-link! opts name language-id))
    {:language name
     :patterns (count patterns)}))

(defn- collection-directories [opts]
  (let [root (io/file (:root opts))
        library (io/file (or (:library opts) (io/file root "library")))
        base-collections (when (.exists library)
                           (filter #(.isDirectory ^File %)
                                   (.listFiles library)))
        extras (map io/file (:dirs opts))]
    (concat base-collections extras)))

(defn- summarize-plan [opts plan]
  (let [summary (->> plan
                     (group-by :action)
                     (reduce (fn [acc [k v]] (assoc acc k (count v))) {}))]
    (println "Diff summary:" summary)
    (when (and (seq plan) (not (:summary-only? opts)))
      (println "Missing in Futon1 (from filesystem):")
      (doseq [entry plan]
        (println " " (pr-str entry))))))

(defn -main [& args]
  (let [opts (-> (parse-args args)
                 (maybe-load-registry))]
    (when (:help? opts)
      (println (usage))
      (System/exit 0))
    (when-not (api-root opts)
      (throw (ex-info "FUTON1_API_BASE is required" {:help (usage)})))
    (let [opts (-> opts
                   (maybe-load-model))]
      (enforce-model-compat! opts)
      (enforce-model-verify! opts)
      (let [root (io/file (:root opts))
            dirs (collection-directories opts)]
        (when-not (seq dirs)
          (throw (ex-info "No pattern directories found" {:root (.getPath root)})))
        (doseq [dir dirs]
          (when (.exists dir)
            (let [result (ingest-collection! opts root dir)]
              (println (format "%s %s (%d patterns)"
                               (cond
                                 (:diff? opts) "Checked"
                                 (:dry-run? opts) "Dry-run"
                                 :else "Synced")
                               (:language result)
                               (:patterns result))))))
        (when (report? opts)
          (summarize-plan opts @(:plan opts)))))))
