(ns futon3.lab.enrichment
  "Lightweight enrichment helpers for /lab endpoints.

  This is intentionally conservative: it extracts sigils from content and
  delegates to pattern-hints for nearest patterns + cues. Embeddings and
  proposals are left empty for now."
  (:require [clojure.string :as str]
            [futon3.pattern-hints :as hints]))

(def ^:private sigil-token-re #"[^\s\[\]]+/[^\s\[\]]+")

(defn- normalize-sigil-part [value]
  (some-> value
          (str/replace #"[\uFE0E\uFE0F\u200D]" "")
          str/trim))

(defn- extract-sigils
  [content]
  (->> (re-seq sigil-token-re (or content ""))
       (keep (fn [token]
               (let [[emoji hanzi] (str/split token #"/" 2)
                     emoji (normalize-sigil-part emoji)
                     hanzi (normalize-sigil-part hanzi)]
                 (when (and (seq emoji) (seq hanzi))
                   {:emoji emoji :hanzi hanzi}))))
       vec))

(defn enrich
  [content & {:keys [pattern-limit use-portal namespace]
              :or {pattern-limit 5 use-portal true}}]
  (let [sigils (extract-sigils content)
        hint-opts (cond-> {:sigils sigils
                           :pattern-limit pattern-limit}
                    use-portal (assoc :portal-namespace namespace))
        hint (if (seq sigils)
               (hints/hints hint-opts)
               {:patterns [] :fruits [] :paramitas []})]
    {:patterns (vec (:patterns hint))
     :cues {:fruits (vec (:fruits hint))
            :paramitas (vec (:paramitas hint))}
     :embedding nil
     :proposals []
     :sigils sigils}))

(defn recurring-patterns
  [& {:keys [min-count min-sessions]
      :or {min-count 3 min-sessions 2}}]
  (let [_ min-count
        _ min-sessions]
    []))
