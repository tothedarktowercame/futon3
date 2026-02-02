#!/usr/bin/env bb
(ns scripts.devmap-kanban
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(def ^:private default-width 36)
(def ^:private ansi-reset "\u001b[0m")

(defn- usage []
  (str/join
   \newline
   ["Usage: bb scripts/devmap_kanban.clj [--dir DIR] [--futon fN] [--cols N] [--width N] [--max N]"
    ""
    "Examples:"
    "  bb scripts/devmap_kanban.clj"
    "  bb scripts/devmap_kanban.clj --futon f5 --cols 2 --width 44"
    "  bb scripts/devmap_kanban.clj --dir holes --max 12"
    ""
    "Notes:"
    "  --max limits cards per futon column."
    "  --no-color disables ANSI coloring."
    "  --micro shows sigils in title instead of English names."
    "  --micro-words uses toki words for emoji in sigils."
    "  --micro-ascii uses ASCII-friendly sigils (words + U+XXXX hanzi)."
    "  --ruler prints a width ruler per column."
    "  --debug-width appends computed line widths."]))

(defn- parse-int [s]
  (try
    (Long/parseLong s)
    (catch Exception _ nil)))

(defn- parse-args [args]
  (loop [opts {:dir "holes"
               :futon nil
               :cols nil
               :width default-width
               :max nil
               :color? true
               :micro? false
               :micro-words? false
               :micro-ascii? false
               :ruler? false
               :debug-width? false}
         remaining args]
    (if (seq remaining)
      (case (first remaining)
        "--dir" (recur (assoc opts :dir (second remaining)) (nnext remaining))
        "--futon" (recur (assoc opts :futon (second remaining)) (nnext remaining))
        "--cols" (recur (assoc opts :cols (parse-int (second remaining))) (nnext remaining))
        "--width" (recur (assoc opts :width (parse-int (second remaining))) (nnext remaining))
        "--max" (recur (assoc opts :max (parse-int (second remaining))) (nnext remaining))
        "--no-color" (recur (assoc opts :color? false) (rest remaining))
        "--micro" (recur (assoc opts :micro? true) (rest remaining))
        "--micro-words" (recur (assoc opts :micro? true :micro-words? true) (rest remaining))
        "--micro-ascii" (recur (assoc opts :micro? true :micro-ascii? true) (rest remaining))
        "--ruler" (recur (assoc opts :ruler? true) (rest remaining))
        "--debug-width" (recur (assoc opts :debug-width? true) (rest remaining))
        "-h" (do (println (usage)) (System/exit 0))
        "--help" (do (println (usage)) (System/exit 0))
        (throw (ex-info (str "Unknown option: " (first remaining)) {:args remaining})))
      opts)))

(def ^:private proto-header-re
  (re-pattern "^!\\s+instantiated-by:\\s+Prototype\\s+(\\d+)\\s+—\\s+(.*)$"))

(def ^:private sigil-block-re
  (re-pattern "\\[(.*?)\\]"))

(def ^:private tokizh-path
  (delay
    (-> (fs/path *file*)
        fs/parent
        fs/parent
        (fs/path "resources" "tokizh" "tokizh.org")
        str)))

(defn- load-tokizh []
  (let [path @tokizh-path]
    (if (fs/exists? path)
      (->> (str/split-lines (slurp path))
           (filter #(str/starts-with? (str/trim %) "|"))
           (map (fn [line]
                  (let [cols (map str/trim (str/split line #"\|"))
                        emoji (nth cols 1 nil)
                        word (nth cols 2 nil)]
                    (when (and emoji word (not (str/blank? emoji)) (not (str/blank? word)))
                      [emoji word]))))
           (remove nil?)
           (into {}))
      {})))

(def ^:private emoji->word (delay (load-tokizh)))

(defn- emoji-word [emoji]
  (get @emoji->word emoji))

(defn- codepoint [s]
  (when (and s (seq s))
    (Character/codePointAt s 0)))

(defn- codepoint-label [s]
  (when-let [cp (codepoint s)]
    (format "U+%04X" cp)))

(defn- render-sigil [token {:keys [micro-words? micro-ascii?]}]
  (let [[emoji hanzi] (str/split token #"/" 2)
        word (emoji-word emoji)
        emoji-text (cond
                     micro-ascii? (or word (codepoint-label emoji) "?")
                     micro-words? (or word emoji)
                     :else emoji)
        hanzi-text (if micro-ascii?
                     (or (codepoint-label hanzi) "?")
                     hanzi)]
    (if hanzi-text
      (str emoji-text "/" hanzi-text)
      (or emoji-text token))))

(defn- futon-id [path]
  (when-let [m (re-find #"futon(\d+[A-Za-z0-9]*)" (fs/file-name path))]
    (str "f" (second m))))

(defn- strip-sigils [title]
  (-> title
      (str/replace #"\s+\[.*$" "")
      str/trim))

(defn- extract-sigils [title]
  (when-let [[_ sigils] (re-find sigil-block-re title)]
    (->> (str/split sigils #"\s+")
         (remove str/blank?)
         vec)))

(defn- parse-depends [line]
  (when-let [[_ body] (re-find #":depends-on\s+\[(.*?)\]" line)]
    (let [items (->> (str/split body #"[\\s,]+")
                     (remove str/blank?)
                     vec)]
      (when (seq items) items))))

(defn- parse-maturity [line]
  (when-let [[_ m] (re-find #":maturity\s+:([A-Za-z0-9_-]+)" line)]
    (keyword m)))

(defn- parse-block [lines]
  (let [maturity (some parse-maturity lines)
        depends (some parse-depends lines)
        evidence (count (filter #(re-find #"evidence\[" %) lines))
        next-steps (count (filter #(re-find #"(?:^|\s)next\[" %) lines))]
    {:maturity maturity
     :depends depends
     :evidence evidence
     :next-steps next-steps}))

(defn- parse-devmap [path]
  (let [lines (str/split-lines (slurp path))
        futon (futon-id path)]
    (loop [remaining lines
           current nil
           acc []]
      (if (empty? remaining)
        (cond-> acc
          current (conj current))
        (let [line (first remaining)]
          (if-let [[_ n title] (re-find proto-header-re line)]
            (let [block (parse-block (if current (:lines current) []))
                  current* {:futon futon
                            :number (parse-int n)
                            :title (strip-sigils title)
                            :sigils (extract-sigils title)
                            :lines []}
                  acc* (cond-> acc
                         current (conj (merge current block)))]
              (recur (rest remaining) current* acc*))
            (recur (rest remaining)
                   (if current
                     (update current :lines conj line)
                     current)
                   acc)))))))

(defn- format-title [card micro? sigil-opts]
  (let [id (format "%s/p%s" (:futon card) (:number card))
        sigil-text (when (seq (:sigils card))
                     (str/join " " (map #(render-sigil % sigil-opts) (:sigils card))))]
    (if micro?
      [id (or sigil-text (:title card))]
      [(str id " " (:title card))])))

(defn- strip-ansi [s]
  (str/replace (or s "") #"\u001b\[[0-9;]*m" ""))

(defn- combining? [cp]
  (or (<= 0x0300 cp 0x036F)
      (<= 0x1AB0 cp 0x1AFF)
      (<= 0x1DC0 cp 0x1DFF)
      (<= 0x20D0 cp 0x20FF)
      (<= 0xFE20 cp 0xFE2F)))

(defn- zero-width? [cp]
  (or (= cp 0x200D)
      (<= 0x200B cp 0x200F)
      (<= 0xFE00 cp 0xFE0F)
      (<= 0xE0100 cp 0xE01EF)
      (combining? cp)))

(defn- wide? [cp]
  (or (= cp 0x2329)
      (= cp 0x232A)
      (<= 0x1100 cp 0x115F)
      (<= 0x2E80 cp 0xA4CF)
      (<= 0xAC00 cp 0xD7A3)
      (<= 0xF900 cp 0xFAFF)
      (<= 0xFE10 cp 0xFE19)
      (<= 0xFE30 cp 0xFE6F)
      (<= 0xFF00 cp 0xFF60)
      (<= 0xFFE0 cp 0xFFE6)
      (<= 0x1F000 cp 0x1FAFF)))

(defn- codepoint-width [cp]
  (cond
    (zero-width? cp) 0
    (wide? cp) 2
    :else 1))

(defn- display-width [s]
  (let [s (strip-ansi s)]
    (loop [i 0
           total 0]
      (if (< i (.length ^String s))
        (let [cp (Character/codePointAt s i)
              step (Character/charCount cp)]
          (recur (+ i step) (+ total (codepoint-width cp))))
        total))))

(defn- trunc [s maxlen]
  (if (<= (display-width s) maxlen)
    s
    (let [limit (max 0 (- maxlen 3))
          s (strip-ansi s)]
      (loop [i 0
             acc ""
             width 0]
        (if (or (>= width limit) (>= i (.length ^String s)))
          (str acc "...")
          (let [cp (Character/codePointAt s i)
                step (Character/charCount cp)
                w (codepoint-width cp)]
            (if (> (+ width w) limit)
              (str acc "...")
              (recur (+ i step)
                     (str acc (String. (Character/toChars cp)))
                     (+ width w)))))))))

(defn- pad [s width]
  (let [s (or s "")
        visible (display-width s)]
    (str s (apply str (repeat (max 0 (- width visible)) " ")))))

(def ^:private maturity-colors
  {"settled" "\u001b[32m"
   "active" "\u001b[35m"
   "greenfield" "\u001b[33m"
   "stub" "\u001b[33m"})

(defn- colorize-maturity [maturity color?]
  (let [label (or maturity "-")
        color (get maturity-colors label)]
    (if (and color? color)
      (str color label ansi-reset)
      label)))

(defn- maturity-line [label inner color?]
  (let [plain (str "maturity: " label)
        truncated (trunc plain inner)]
    (if (and color? (= plain truncated))
      (str "maturity: " (colorize-maturity label true))
      truncated)))

(defn- card-lines [card width color? micro? sigil-opts]
  (let [inner (- width 2)
        depends (or (some->> (:depends card) (str/join " ")) "-")
        maturity (or (some-> (:maturity card) name) "-")
        header-lines (format-title card micro? sigil-opts)
        header-lines (if (sequential? header-lines) header-lines [header-lines])
        header-lines (map #(trunc % inner) header-lines)
        header-line1 (first header-lines)
        header-line2 (second header-lines)
        line2 (maturity-line maturity inner color?)
        line3 (trunc (str "depends: " depends) inner)
        line4 (trunc (format "evidence: %d  next: %d"
                             (or (:evidence card) 0)
                             (or (:next-steps card) 0))
                     inner)]
    (cond-> [(str "+" (apply str (repeat inner "-")) "+")
             (str "|" (pad header-line1 inner) "|")]
      header-line2 (conj (str "|" (pad header-line2 inner) "|"))
      true (conj (str "|" (pad line2 inner) "|")
                 (str "|" (pad line3 inner) "|")
                 (str "|" (pad line4 inner) "|")
                 (str "+" (apply str (repeat inner "-")) "+")))))

(defn- blank-line [width]
  (apply str (repeat width " ")))

(defn- column-lines [cards width color? micro? sigil-opts]
  (->> cards
       (map #(card-lines % width color? micro? sigil-opts))
       (interpose [(blank-line width)])
       (apply concat)
       vec))

(defn- futon-sort-key [fid]
  (if-let [m (re-matches #"f(\d+)([A-Za-z0-9]*)" (str fid))]
    [(Long/parseLong (second m)) (str/lower-case (nth m 2 ""))]
    [Long/MAX_VALUE (str fid)]))

(defn- ruler-line [width]
  (apply str (map #(char (+ (int \0) (mod % 10))) (range 1 (inc width)))))

(defn- fmt-widths [vals]
  (str/join "," (map str vals)))

(defn- render-columns [columns width {:keys [ruler? debug-width?]}]
  (when ruler?
    (println (str/join "  " (repeat (count columns) (ruler-line width))))
    (when debug-width?
      (println (str "WIDTHS: " (fmt-widths (repeat (count columns) width))))))
  (let [height (apply max 0 (map count columns))]
    (doseq [i (range height)]
      (let [cells (for [col columns]
                    (pad (or (get col i) "") width))
            line (str/join "  " cells)]
        (if debug-width?
          (println line (str "  ⟨w=" (display-width line) "⟩"))
          (println line))))
    (println)))

(defn -main [& args]
  (let [{:keys [dir futon cols width color? micro? micro-words? micro-ascii? ruler? debug-width?] :as opts} (parse-args args)
        max-cards (:max opts)
        color? (and color? (not (System/getenv "NO_COLOR")))
        sigil-opts {:micro-words? micro-words?
                    :micro-ascii? micro-ascii?}
        files (->> (fs/list-dir dir)
                   (map str)
                   (filter #(str/ends-with? % ".devmap"))
                   sort)
        files (if futon
                (filter #(str/includes? % (str "futon" (subs futon 1))) files)
                files)
        cards (->> files
                   (mapcat parse-devmap)
                   (sort-by (juxt (comp futon-sort-key :futon) :number))
                   (vec))
        cards (if (and max-cards (pos? max-cards))
                (->> cards
                     (group-by :futon)
                     (map (fn [[fid items]]
                            [fid (vec (take max-cards items))]))
                     (sort-by (comp futon-sort-key first))
                     (mapcat second)
                     vec)
                cards)]
    (if (seq cards)
      (let [cards-by-futon (->> cards
                                (group-by :futon)
                                (sort-by (comp futon-sort-key first)))
            cols (or cols (count cards-by-futon) 1)
            cols (max 1 cols)
            width (max 20 (or width default-width))]
        (doseq [chunk (partition-all cols cards-by-futon)]
          (let [columns (map (fn [[_ items]]
                               (column-lines items width color? micro? sigil-opts))
                             chunk)]
            (render-columns columns width {:ruler? ruler?
                                           :debug-width? debug-width?}))))
      (println "No devmap prototypes found."))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
