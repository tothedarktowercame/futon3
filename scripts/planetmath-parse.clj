#!/usr/bin/env bb
;; Parse PlanetMath LaTeX files into EDN for futon1 ingestion
;;
;; Usage:
;;   bb scripts/planetmath-parse.clj <dir> [output.edn]
;;
;; Example:
;;   bb scripts/planetmath-parse.clj ~/code/planetmath/18_Category_theory_homological_algebra
;;   bb scripts/planetmath-parse.clj ~/code/planetmath/18_Category_theory_homological_algebra entries.edn

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.edn :as edn])

(defn extract-pmmeta-field [content field]
  "Extract a single pmmeta field value."
  (let [pattern (re-pattern (str "\\\\pm" field "\\{([^}]*)\\}"))]
    (second (re-find pattern content))))

(defn extract-pmmeta-field-multi [content field]
  "Extract multiple occurrences of a pmmeta field (e.g., pmrelated, pmclassification)."
  (let [pattern (re-pattern (str "\\\\pm" field "\\{([^}]*)\\}"))]
    (->> (re-seq pattern content)
         (map second)
         (remove str/blank?)
         vec)))

(defn extract-pmclassification [content]
  "Extract MSC classifications as {msc code} pairs."
  (let [pattern #"\\pmclassification\{([^}]*)\}\{([^}]*)\}"]
    (->> (re-seq pattern content)
         (map (fn [[_ scheme code]] {:scheme scheme :code code}))
         vec)))

(defn extract-body [content]
  "Extract the body content after metadata."
  (let [;; Try to find content after \begin{document}
        doc-match (re-find #"\\begin\{document\}(.*)\\end\{document\}" content)
        ;; Or after \endmetadata
        meta-match (re-find #"\\endmetadata\s*(.*)" content)]
    (cond
      doc-match (str/trim (second doc-match))
      meta-match (str/trim (second meta-match))
      :else nil)))

(defn parse-tex-file [file]
  "Parse a single .tex file into an entry map."
  (let [content (slurp file)
        filename (.getName file)]
    {:entry/id (or (extract-pmmeta-field content "canonicalname")
                   (str/replace filename #"\.tex$" ""))
     :entry/title (extract-pmmeta-field content "title")
     :entry/type (extract-pmmeta-field content "type")
     :entry/created (extract-pmmeta-field content "created")
     :entry/modified (extract-pmmeta-field content "modified")
     :entry/author (extract-pmmeta-field content "owner")
     :entry/msc-codes (extract-pmclassification content)
     :entry/related (extract-pmmeta-field-multi content "related")
     :entry/defines (extract-pmmeta-field-multi content "defines")
     :entry/keywords (->> (re-seq #"%\\pmkeywords\{([^}]*)\}" content)
                          (map second)
                          (remove str/blank?)
                          vec)
     :entry/body (extract-body content)
     :entry/source "planetmath"
     :entry/source-file filename}))

(defn parse-directory [dir]
  "Parse all .tex files in a directory."
  (let [files (->> (io/file dir)
                   file-seq
                   (filter #(and (.isFile %)
                                 (str/ends-with? (.getName %) ".tex"))))]
    (println (format "Parsing %d files from %s..." (count files) dir))
    (->> files
         (map parse-tex-file)
         (filter :entry/id)
         vec)))

(defn -main [& args]
  (let [[dir output] args]
    (when (or (nil? dir) (not (.exists (io/file dir))))
      (println "Usage: bb scripts/planetmath-parse.clj <dir> [output.edn]")
      (System/exit 1))

    (let [entries (parse-directory dir)
          output-file (or output (str (-> dir io/file .getName) ".edn"))]
      (println (format "Parsed %d entries" (count entries)))

      ;; Print sample
      (when (seq entries)
        (println "\nSample entry:")
        (println "  ID:" (:entry/id (first entries)))
        (println "  Title:" (:entry/title (first entries)))
        (println "  Type:" (:entry/type (first entries)))
        (println "  MSC:" (:entry/msc-codes (first entries)))
        (println "  Related:" (take 3 (:entry/related (first entries)))))

      ;; Write output
      (spit output-file (pr-str entries))
      (println (format "\nWritten to %s" output-file)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
