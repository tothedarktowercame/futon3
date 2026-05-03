#!/usr/bin/env bb
;; Extract candidate terms from a vocabulary markdown doc.
;; Source: section headings (## or deeper) + **bolded phrases** + |table| cells.
;; Output: EDN set of {:term :source-doc :first-line}.

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[babashka.fs :as fs])

(def stopwords
  #{"the" "and" "or" "of" "to" "for" "in" "on" "at" "by" "with"
    "from" "is" "are" "was" "were" "be" "been" "being"
    "this" "that" "these" "those" "what" "where" "when" "how"
    "which" "who" "an" "as" "if" "but" "not" "no" "yes"
    "key insight" "next steps" "current status" "summary"
    "note" "todo" "fixme" "see also" "see" "also"
    ;; markdown structural words that bold often
    "before" "after" "during" "within" "across" "between"
    ;; too-generic engineering nouns
    "domain" "system" "code" "structure" "process" "function"
    "module" "package" "service" "platform"
    "loop" "step" "phase" "state" "time" "value" "type"
    "result" "output" "input" "data" "test" "tests"
    "this is where xenotypes live"
    "the outer loop is also aif"})

(defn clean-term [s]
  (-> s
      ;; strip parenthetical glosses: "Observables (o)" → "Observables"
      (str/replace #"\s*\([^)]*\)\s*" "")
      ;; strip leading list/header markers
      (str/replace #"^[\s*#\-•]+" "")
      ;; collapse whitespace
      (str/replace #"\s+" " ")
      str/trim
      str/lower-case))

(defn good-term? [t]
  (and (string? t)
       (let [n (count t)]
         (and (>= n 3) (<= n 50)))
       (re-matches #"[a-z][a-z0-9 \-/&]*" t)
       (not (stopwords t))
       ;; reject pure numbers / single-letter+number forms
       (not (re-matches #"^[0-9]+$" t))))

(defn extract-headings [text]
  (->> (str/split-lines text)
       (keep (fn [ln]
               (when-let [m (re-find #"^#{2,}\s+(.+?)\s*$" ln)]
                 (clean-term (second m)))))))

(defn extract-bold [text]
  (->> (re-seq #"\*\*([^*\n]+?)\*\*" text)
       (map second)
       (map clean-term)))

(defn extract-table-cells [text]
  (->> (str/split-lines text)
       (filter #(re-find #"^\|.*\|" %))
       (mapcat (fn [ln]
                 (->> (str/split ln #"\|")
                      (map clean-term)
                      ;; skip separator rows (---)
                      (remove #(re-matches #"-+" %)))))))

(defn extract-italic
  "Match *term* but not **term** (bold)."
  [text]
  (->> (re-seq #"(?<!\*)\*([^*\n]{2,40})\*(?!\*)" text)
       (map second)
       (map clean-term)))

(defn extract-from-doc [path]
  (let [text (slurp path)
        cands (concat (extract-headings text)
                      (extract-bold text)
                      (extract-italic text)
                      (extract-table-cells text))
        kept  (filter good-term? cands)]
    (into (sorted-set) kept)))

(defn -main [& argv]
  (let [docs argv
        all  (atom (sorted-set))
        per-doc (atom {})]
    (doseq [d docs]
      (let [terms (extract-from-doc d)]
        (swap! all into terms)
        (swap! per-doc assoc d (count terms))
        (println d "→" (count terms) "terms")))
    (println)
    (println "TOTAL UNIQUE:" (count @all))
    (println)
    (println "First 60 alphabetically:")
    (doseq [t (take 60 @all)] (println "  " t))
    (println)
    (println "Last 30 alphabetically:")
    (doseq [t (take-last 30 @all)] (println "  " t))))

(apply -main *command-line-args*)
