#!/usr/bin/env bb
;; Update @sigils in flexiarg files using bridge-derived assignments.
;;
;; The bridge (futon5) produces informed 8-bit encodings for each pattern
;; via MiniLM embeddings + hexagram skeleton. Chops resolves these to
;; hanzi (truth-table-8) and emoji (tokizh). This script writes the
;; results back into the flexiarg files as @sigils lines.
;;
;; Usage:
;;   cd /home/joe/code/futon3
;;   bb -cp src:resources scripts/update_sigils_from_bridge.clj
;;
;; Options:
;;   --dry-run     Show what would change without writing (default)
;;   --write       Actually write changes to flexiarg files
;;   --only NS     Only update patterns in namespace NS (e.g., --only ants)

(require '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[futon3.chops :as chops])

;; =============================================================================
;; ARGS
;; =============================================================================

(defn parse-args [args]
  (loop [args args
         opts {:dry-run true :only nil}]
    (if (empty? args)
      opts
      (let [[flag & more] args]
        (case flag
          "--write" (recur more (assoc opts :dry-run false))
          "--dry-run" (recur more (assoc opts :dry-run true))
          "--only" (recur (rest more) (assoc opts :only (first more)))
          (do (println "Unknown flag:" flag)
              (recur more opts)))))))

;; =============================================================================
;; FLEXIARG UPDATE
;; =============================================================================

(defn find-flexiarg-files
  "Find all .flexiarg files under library/."
  [root only-ns]
  (let [dir (io/file root)
        files (file-seq dir)]
    (->> files
         (filter #(str/ends-with? (.getName %) ".flexiarg"))
         (filter (fn [f]
                   (if only-ns
                     (str/includes? (.getPath f) (str "/" only-ns "/"))
                     true)))
         vec)))

(defn pattern-id-from-path
  "library/foo/bar.flexiarg → foo/bar"
  [f library-root]
  (let [rel (str/replace (.getPath f) (str library-root "/") "")]
    (-> rel
        (str/replace #"\.flexiarg$" "")
        (str/replace #"^library/" ""))))

(defn update-sigils-in-text
  "Replace or insert @sigils line in flexiarg text.
   Returns [new-text changed?]."
  [text sigil-str]
  (let [sigil-line (str "@sigils [" sigil-str "]")
        lines (str/split-lines text)]
    (if-let [idx (first (keep-indexed
                          (fn [i line]
                            (when (str/starts-with? (str/trim line) "@sigils")
                              i))
                          lines))]
      ;; Replace existing @sigils line
      (let [old-line (nth lines idx)
            new-lines (assoc (vec lines) idx sigil-line)]
        (if (= old-line sigil-line)
          [text false]
          [(str/join "\n" new-lines) true]))
      ;; Insert after line 2 (after @title, before first section)
      ;; Find insertion point: after last @-metadata line before first + or blank
      (let [insert-at (or (first (keep-indexed
                                   (fn [i line]
                                     (when (and (> i 0)
                                                (not (str/starts-with? (str/trim line) "@"))
                                                (not (str/blank? line)))
                                       i))
                                   lines))
                          2)
            new-lines (vec (concat (take insert-at lines)
                                   [sigil-line]
                                   (drop insert-at lines)))]
        [(str/join "\n" new-lines) true]))))

;; =============================================================================
;; MAIN
;; =============================================================================

(let [args (parse-args *command-line-args*)
      library-root "/home/joe/code/futon3/library"

      ;; Load bridge assignments via chops
      _ (println "Loading bridge assignments...")
      _ (chops/bridge-assignments)
      meta-info (:meta (chops/bridge-assignments))
      _ (println (str "  " (:n-patterns meta-info) " patterns, "
                      (:n-complete meta-info) " with emoji"))
      _ (println)

      ;; Find flexiarg files
      files (find-flexiarg-files library-root (:only args))
      _ (println (str "Found " (count files) " flexiarg files"
                      (when (:only args) (str " in " (:only args)))))

      ;; Process each file
      results
      (for [f files
            :let [pid (pattern-id-from-path f library-root)
                  assignment (chops/assign-sigil pid)]
            :when assignment]
        (let [text (slurp f)
              sigil-str (:sigil assignment)
              [new-text changed?] (update-sigils-in-text text sigil-str)]
          {:file f
           :pattern-id pid
           :sigil sigil-str
           :emoji (:emoji assignment)
           :tokipona (:tokipona assignment)
           :bits (:bits assignment)
           :confidence (:confidence assignment)
           :changed? changed?
           :new-text new-text}))

      matched (vec results)
      changed (filterv :changed? matched)
      unchanged (filterv (complement :changed?) matched)
      unmatched-count (- (count files) (count matched))]

  ;; Report
  (println)
  (println "=== Sigil Update Report ===")
  (println (str "Files scanned:     " (count files)))
  (println (str "Bridge matches:    " (count matched)))
  (println (str "  Would change:    " (count changed)))
  (println (str "  Already correct: " (count unchanged)))
  (println (str "No bridge match:   " unmatched-count))
  (println)

  (when (seq changed)
    (println "--- Changes ---")
    (doseq [r (take 40 (sort-by :pattern-id changed))]
      (printf "  %-45s → %s%s%n"
              (:pattern-id r)
              (:sigil r)
              (if (:tokipona r)
                (str " (" (:tokipona r) ")")
                "")))
    (when (> (count changed) 40)
      (println (str "  ... and " (- (count changed) 40) " more")))
    (println))

  ;; Write if --write
  (if (:dry-run args)
    (println "Dry run — no files modified. Use --write to apply.")
    (do
      (println "Writing changes...")
      (doseq [r changed]
        (spit (:file r) (:new-text r)))
      (println (str "Updated " (count changed) " files.")))))
