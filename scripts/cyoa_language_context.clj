(ns scripts.cyoa-language-context
  "Generate a whole-language CYOA payload for missing context lines in flexiarg patterns."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import (java.time LocalDateTime)
           (java.time.format DateTimeFormatter)))

(def default-dir "library")
(def default-output-dir "holes/logs/cyoa")
(def tatami-in-marker "FROM-TATAMI-EDN")

(def timestamp-format (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss"))

(defn now-stamp []
  (.format timestamp-format (LocalDateTime/now)))

(defn ensure-parent [path]
  (io/make-parents (io/file path)))

(defn write-edn [path data]
  (ensure-parent path)
  (with-open [writer (io/writer path)]
    (binding [*out* writer]
      (pprint/pprint data))))

(defn abs-path [path]
  (.getAbsolutePath (io/file path)))

(defn slurp-when [path]
  (when path
    (let [file (io/file path)]
      (when (.exists file)
        (slurp file)))))

(defn flexiarg-files [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".flexiarg"))
       (map #(.getPath ^java.io.File %))
       sort
       vec))

(defn block-start
  [line]
  (when-let [[_ _ arg] (re-find #"^@(arg|flexiarg)\s+(\S+)" line)]
    arg))

(defn extract-title
  [lines]
  (some (fn [line]
          (when-let [[_ title] (re-find #"^@title\s+(.+)$" line)]
            title))
        lines))

(defn block-has-context?
  [lines]
  (boolean (some #(re-find #"^\s*\+\s*context:" %) lines)))

(defn parse-flexiarg
  [text]
  (let [lines (str/split-lines (or text ""))]
    (loop [remaining lines
           preamble []
           blocks []
           current nil]
      (if (empty? remaining)
        {:preamble preamble
         :blocks (cond-> blocks
                   current (conj (update current :lines vec)))}
        (let [line (first remaining)
              rest-lines (rest remaining)
              arg (block-start line)]
          (if arg
            (recur rest-lines
                   preamble
                   (cond-> blocks
                     current (conj (update current :lines vec)))
                   {:arg arg
                    :lines [line]})
            (if current
              (recur rest-lines
                     preamble
                     blocks
                     (update current :lines conj line))
              (recur rest-lines
                     (conj preamble line)
                     blocks
                     current))))))))

(defn build-message
  [payload]
  (str "Here is the FROM-TATAMI-EDN block for this turn. Use it directly.\n"
       "---" tatami-in-marker "---\n"
       (pr-str payload) "\n"
       "---END-" tatami-in-marker "---"))

(defn normalize-context
  [s]
  (-> (or s "")
      str/trim
      (str/replace #"\s+" " ")))

(defn insert-context-line
  [lines context]
  (if (block-has-context? lines)
    {:lines lines :applied? false :warning "Context already present"}
    (let [plus-idx (first (keep-indexed (fn [idx line]
                                          (when (re-find #"^\s*\+\s*[A-Za-z0-9-]+:" line)
                                            idx))
                                        lines))
          indent (or (some->> lines
                              (keep #(second (re-find #"^(\s*)\+" %)))
                              first)
                     "  ")
          line (str indent "+ context: " (normalize-context context))
          target-idx (or plus-idx (count lines))
          new-lines (vec (concat (subvec (vec lines) 0 target-idx)
                                 [line]
                                 (subvec (vec lines) target-idx)))]
      {:lines new-lines :applied? true})))

(defn apply-contexts-to-file
  [path contexts]
  (let [raw (slurp path)
        parsed (parse-flexiarg raw)
        ctx-by-arg (into {} (map (juxt :arg :context) contexts))
        results (mapv (fn [block]
                        (if-let [context (get ctx-by-arg (:arg block))]
                          (let [result (insert-context-line (:lines block) context)]
                            (assoc block
                                   :lines (:lines result)
                                   :applied? (:applied? result)
                                   :warning (:warning result)))
                          (assoc block :applied? false)))
                      (:blocks parsed))
        rebuilt (str/join "\n" (concat (:preamble parsed)
                                       (mapcat :lines results)))
        final-text (if (str/ends-with? raw "\n")
                     (str rebuilt "\n")
                     rebuilt)
        applied (filter :applied? results)
        warnings (keep :warning results)]
    {:text final-text
     :applied (mapv :arg applied)
     :warnings (vec warnings)}))

(defn parse-args [args]
  (loop [opts {:dir default-dir
               :output-dir default-output-dir}
         args args]
    (if-let [arg (first args)]
      (case arg
        "--dir" (recur (assoc opts :dir (second args)) (nnext args))
        "--output-dir" (recur (assoc opts :output-dir (second args)) (nnext args))
        "--include-existing" (recur (assoc opts :include-existing true) (next args))
        "--apply" (recur (assoc opts :apply (second args)) (nnext args))
        "--help" (recur (assoc opts :help true) (next args))
        (throw (ex-info (str "Unknown argument: " arg) {:arg arg})))
      opts)))

(defn usage []
  (println "Usage: clj -M -m scripts.cyoa-language-context [options]")
  (println "Options:")
  (println "  --dir PATH             Directory of .flexiarg files (default library)")
  (println "  --output-dir PATH      Output directory (default holes/logs/cyoa)")
  (println "  --include-existing     Include patterns that already have context")
  (println "  --apply PATH           EDN file with {:contexts [...]} to apply")
  (println "  --help                 Show help"))

(defn load-contexts [path]
  (when-let [raw (slurp-when path)]
    (edn/read-string raw)))

(defn -main [& args]
  (let [{:keys [dir output-dir include-existing apply help]} (parse-args args)]
    (when help
      (usage)
      (System/exit 0))
    (let [files (flexiarg-files dir)]
      (when (empty? files)
        (throw (ex-info (str "No .flexiarg files found in " dir) {:dir dir})))
      (when apply
        (let [data (load-contexts apply)
              contexts (or (:contexts data) [])
              grouped (group-by :path contexts)
              results (reduce (fn [acc [path ctxs]]
                                (let [result (apply-contexts-to-file path ctxs)]
                                  (spit path (:text result))
                                  (update acc :results conj
                                          {:path path
                                           :applied (:applied result)
                                           :warnings (:warnings result)})))
                              {:results []}
                              grouped)
              stamp (now-stamp)
              log-path (abs-path (io/file output-dir (str "context-" stamp "-apply-log.edn")))]
          (write-edn log-path (assoc results :input apply))
          (println "Applied contexts; log:" log-path)
          (System/exit 0)))
      (let [patterns (->> files
                          (mapcat (fn [path]
                                    (let [raw (slurp path)
                                          parsed (parse-flexiarg raw)]
                                      (for [block (:blocks parsed)]
                                        (let [title (extract-title (:lines block))
                                              has-context (block-has-context? (:lines block))]
                                          {:path path
                                           :arg (:arg block)
                                           :title title
                                           :has-context has-context
                                           :block (str/join "\n" (:lines block))})))))
                          (filter #(or include-existing (not (:has-context %))))
                          vec)
            payload {:generated-at (System/currentTimeMillis)
                     :dir dir
                     :patterns patterns}
            stamp (now-stamp)
            base (str "context-" stamp)
            input-path (abs-path (io/file output-dir (str base "-input.edn")))
            message-path (abs-path (io/file output-dir (str base "-message.txt")))
            log-path (abs-path (io/file output-dir (str base "-log.edn")))]
        (write-edn input-path payload)
        (spit message-path (build-message payload))
        (write-edn log-path {:timestamp stamp
                             :dir dir
                             :pattern-count (count patterns)
                             :input input-path
                             :message message-path})
        (println "Wrote context payload to" input-path)
        (println "Wrote message to" message-path)
        (println "Wrote log to" log-path)))))

