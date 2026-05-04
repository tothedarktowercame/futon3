(ns flexiarg-projection
  "Substrate-2 projector for .flexiarg pattern files. Reads the
   `@flexiarg <ns>/<name>` header to derive the pattern's qname,
   plus optional `@title`, `! conclusion:`, and the Toulmin-style
   slot fields (IF, HOWEVER, THEN, BECAUSE) as props. v0 emits one
   :var (kind=flexiarg) per file."
  (:require [clojure.string :as str]
            [futon.flexiarg.projection :as projection]))

(def src-exts #{"flexiarg" "multiarg"})

(defn- packet->header
  [packet]
  (when-let [pattern-id (:pattern/id packet)]
    (when-let [i (str/last-index-of pattern-id "/")]
      (let [ns-part (subs pattern-id 0 i)
            nm (subs pattern-id (inc i))]
        {:ns (str "flexiarg." (str/replace ns-part "/" "."))
         :name nm
         :qname (str "flexiarg." (str/replace ns-part "/" ".") "/" nm)}))))

(defn- has-toulmin-slots? [packet]
  ;; A "well-formed" flexiarg uses Toulmin slots IF/HOWEVER/THEN/BECAUSE.
  ;; Treat presence of all four as the doc-discipline signal.
  (let [keys (into #{} (map :name-key) (:pattern/clauses packet))]
    (every? keys ["if" "however" "then" "because"])))

(defn- collect-symbols
  "v0: collect explicit pattern references from `library/<ns>/<name>`
   path-shaped strings inside the body. These are the closest analogue
   to body-syms in clj/el — pattern→pattern citations."
  [packet]
  (let [body-text (str/join "\n\n" (map :text (:pattern/clauses packet)))
        inline-refs (re-seq #"library/[\w./_-]+/[\w._-]+" body-text)
        explicit-refs (:pattern/references packet)]
    (into #{}
          (keep (fn [s]
                  (let [ref (str/replace s #"^library/" "")
                        i (str/last-index-of ref "/")]
                    (when i
                      (let [ns (subs ref 0 i)
                            nm (subs ref (inc i))]
                        (symbol (str "flexiarg."
                                     (str/replace ns "/" ".")
                                     "/"
                                     nm)))))))
          (concat explicit-refs inline-refs))))

(defn collect-file
  "Project one .flexiarg file into the substrate-2 metadata shape."
  [path]
  (let [packets (projection/parse-file path)
        ok-packets (filter #(= :ok (:pattern/status %)) packets)
        headers (keep packet->header ok-packets)
        first-header (first headers)]
    (when first-header
      {:ns (:ns first-header)
       :aliases {}
       :is-test? false
       :tests []
       :vars (mapv (fn [packet]
                     (let [header (packet->header packet)
                           doc-quality (has-toulmin-slots? packet)
                           body-syms (collect-symbols packet)]
                       {:vertex/type :var
                        :var/ns (:ns header)
                        :var/name (:name header)
                        :var/qname (:qname header)
                        :var/kind "flexiarg"
                        :var/has-doc (or (some? (:pattern/title packet))
                                         (some? (:pattern/conclusion packet))
                                         doc-quality)
                        :var/syms body-syms}))
                   ok-packets)})))
