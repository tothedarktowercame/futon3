(ns retrodiction-items
  "The BLIND EXTRACTOR of worklist row `:LA7`
   (futon3/holes/labs/library-contract/worklist.edn) -- the RETRODICTION class of
   P-validated-R5.md:699-707.

   WHAT THE ROW ASKS FOR, and why this file exists at all.  `:LA7` constructs a
   policy from a COMPLETED work item's STATEMENT as recorded and compares it
   against the resolution the ledger actually records, under two disciplines:
   the constructor is given the statement ONLY, and the comparison is made by
   someone who did not run the constructor.  A ledger row holds the statement
   and the resolution in the SAME map, so the constructor cannot be handed the
   ledger.  This file is the split: it reads both halves and writes them to two
   files, and only one of them is given to the constructor.

     --blind        -> checks/retrodiction-items.edn        (statements only)
     --resolutions  -> checks/retrodiction-resolutions.edn  (the recorded outcome)
     --verify       -> re-derive both and compare against what is on disk

   THE TWO FILES ARE WRITTEN AT DIFFERENT TIMES ON PURPOSE.  `--blind` runs and
   commits BEFORE the constructor is dispatched; `--resolutions` runs and commits
   AFTER the constructor's own commit lands.  So the resolution file DID NOT
   EXIST IN THE REPOSITORY while the constructor ran, and git history is the
   check on that -- a stronger statement than a promise, and the only mechanical
   one available, since both ledgers are readable on disk to anything with a
   shell.  What is NOT claimed: that the constructor could not have opened
   `futon2/holes/labs/wm-contract/worklist.edn` itself.  Two things bound that
   leak and both are checkable after the fact: the commit order above, and the
   cue licence of `decisions.edn :cue-licence`, which in its ledger-row form
   (see `:statement-address` below) strikes any cue that is not a word of the
   statement -- so a cue read off a resolution cannot survive into a seed.

   SELECTION IS MECHANICAL AND FIXED BEFORE ANY CONTENT WAS READ.  The thresholds
   below were chosen from a table of STATEMENT and EVIDENCE character counts, not
   from what any row says; the spacing rule then takes evenly spaced indices of
   whatever qualifies.  This matters because the author of this file has read
   some of the resolutions -- it is what disqualifies that author from being the
   constructor -- and a hand-picked item set would be a selection made with that
   knowledge.  `:selection` in each output records the rule and the denominators
   it was applied to, re-measured on every run rather than carried forward."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

;; ---------------------------------------------------------------------------
;; the two ledgers
;; ---------------------------------------------------------------------------

(def wm-ledger "../futon2/holes/labs/wm-contract/worklist.edn")
(def tickets-index "../futon3c/holes/tickets/tickets-index.edn")
(def tickets-dir "../futon3c/holes/tickets")

(def blind-path "checks/retrodiction-items.edn")
(def resolutions-path "checks/retrodiction-resolutions.edn")

;; ---------------------------------------------------------------------------
;; the selection rule -- pre-registered, and applied by code rather than by hand
;; ---------------------------------------------------------------------------

(def wm-completed #{:done :done-unreviewed})
(def ticket-completed #{:done-in-fact :superseded})

(def min-statement-chars
  "A statement short enough to be a single clause cannot carry a Tension with
   more than one clause, and `antecedent-holds?` needs an IF-hit and a
   HOWEVER-hit to fire at all.  200 is the length at which a row states a
   situation and a counter-force rather than a bare fact."
  200)

(def min-resolution-chars
  "The resolution half has to be substantive enough to be COMPARED against.  A
   14-character `:evidence` (several :C rows carry only a sha) records that the
   work happened and nothing about what was done."
  500)

(def wm-take
  "How many wm-contract rows enter the packet."
  3)

(def tickets-take
  "How many futon3c tickets enter the packet."
  2)

(defn evenly-spaced
  "Indices `(floor (i*n/k))` for i in 0..k-1 -- k items spread across the whole
   qualifying list in ledger order, so the packet is not the first k rows of one
   section of one ledger."
  [n k]
  (if (or (zero? n) (zero? k))
    []
    (vec (distinct (for [i (range (min k n))] (quot (* i n) k))))))

;; ---------------------------------------------------------------------------
;; digests
;; ---------------------------------------------------------------------------

(defn sha256 [^String s]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest md (.getBytes s "UTF-8"))))))

;; ---------------------------------------------------------------------------
;; wm-contract rows
;; ---------------------------------------------------------------------------

(defn- wm-rows []
  (:items (edn/read-string (slurp wm-ledger))))

(defn- wm-qualifies? [row]
  (and (contains? wm-completed (:status row))
       (not= :J (:class row))
       (>= (count (str (:statement row))) min-statement-chars)
       (>= (count (str (:evidence row))) min-resolution-chars)))

(defn- wm-selected []
  (let [all (wm-rows)
        completed (filterv #(contains? wm-completed (:status %)) all)
        qualifying (filterv wm-qualifies? all)
        idxs (evenly-spaced (count qualifying) wm-take)]
    {:ledger wm-ledger
     :rows-in-ledger (count all)
     :completed (count completed)
     :qualifying (count qualifying)
     :qualifying-ids (mapv (comp name :id) qualifying)
     :indices idxs
     :selected (mapv #(nth qualifying %) idxs)}))

;; ---------------------------------------------------------------------------
;; futon3c tickets
;; ---------------------------------------------------------------------------

(defn- goal-span
  "The `## Goal` section of a ticket: [first-line last-line] 1-indexed inclusive,
   or nil.  A ticket's Goal is written when the ticket is opened and its
   `**Status (triaged ...)**` line when it is triaged, so the two halves of a
   ticket are separated in TIME the way a worklist row's :statement and
   :evidence are."
  [lines]
  (when-let [gi (first (keep-indexed (fn [i l] (when (re-matches #"##\s+Goal\s*" l) i)) lines))]
    (let [nxt (or (first (keep-indexed (fn [i l] (when (and (> i gi) (str/starts-with? l "## ")) i))
                                       lines))
                  (count lines))]
      [(+ gi 2) nxt])))

(defn- ticket-body [row]
  (let [f (io/file tickets-dir (:ticket row))]
    (when (.exists f)
      (let [lines (vec (str/split-lines (slurp f)))
            span (goal-span lines)
            goal (when span (str/trim (str/join "\n" (subvec lines (dec (first span)) (second span)))))
            status (first (filter #(str/starts-with? % "**Status") lines))]
        {:file (str tickets-dir "/" (:ticket row))
         :lines lines
         :goal-span span
         :goal goal
         :title (first lines)
         :status-line status}))))

(defn- ticket-qualifies? [row]
  (let [b (ticket-body row)]
    (and (contains? ticket-completed (:status row))
         (some? (:goal b))
         (>= (count (:goal b)) min-statement-chars)
         (>= (count (str (:status-line b))) 100))))

(defn- ticket-selected []
  (let [all (edn/read-string (slurp tickets-index))
        completed (filterv #(contains? ticket-completed (:status %)) all)
        qualifying (filterv ticket-qualifies? all)
        idxs (evenly-spaced (count qualifying) tickets-take)]
    {:ledger tickets-index
     :rows-in-ledger (count all)
     :completed (count completed)
     :qualifying (count qualifying)
     :qualifying-ids (mapv :ticket qualifying)
     :indices idxs
     :selected (mapv #(nth qualifying %) idxs)}))

;; ---------------------------------------------------------------------------
;; the two halves of an item
;; ---------------------------------------------------------------------------

(defn- wm-item-id [row] (keyword "wm" (name (:id row))))
(defn- ticket-item-id [row]
  (keyword "ticket" (str/replace (str/replace (:ticket row) #"^T-" "") #"\.md$" "")))

(defn- wm-blind [row]
  (let [stmt (str (:statement row))]
    (sorted-map
     :item (wm-item-id row)
     :ledger wm-ledger
     :row-id (:id row)
     :statement stmt
     :statement-sha256 (sha256 stmt)
     ;; THE SECOND FORM OF THE CUE LICENCE, which `decisions.edn :cue-licence
     ;; :changes-it` says it does not supply: a ledger row has no line span, so
     ;; the cited span is a KEY PATH into an EDN map.  A cue must occur,
     ;; case-insensitively, in the value at this address.
     :statement-address (sorted-map :kind :edn-key-path
                                    :file wm-ledger
                                    :row-id (:id row)
                                    :key :statement))))

(defn- ticket-blind [row]
  (let [b (ticket-body row)
        stmt (str (:title b) "\n\n" (:goal b))]
    (sorted-map
     :item (ticket-item-id row)
     :ledger tickets-index
     :row-id (:ticket row)
     :statement stmt
     :statement-sha256 (sha256 stmt)
     :statement-address (sorted-map :kind :title-plus-line-span
                                    :file (:file b)
                                    :title-line 1
                                    :lines (:goal-span b)))))

(defn- shingles
  "Normalised word n-grams.  Whitespace and case collapsed, punctuation kept --
   two texts that share a punctuated pointer share it verbatim or not at all."
  [n text]
  (let [ws (str/split (str/lower-case (str/trim (str/replace (str text) #"\s+" " "))) #" ")]
    (if (< (count ws) n)
      #{}
      (into #{} (map #(str/join " " %)) (partition n 1 ws)))))

(defn- distinctive-tokens
  "Tokens that carry a pointer, an identifier or a number -- a token containing a
   digit, or one containing `.` `:` `_` or `/` between word characters.  These
   are the strings a row cannot state before its work is done."
  [text]
  (into (sorted-set)
        (filter #(or (re-find #"\d" %) (re-find #"\w[./:_]\w" %)))
        (str/split (str/lower-case (str text)) #"[\s,;()\[\]\"]+")))

(defn- overlap
  "THE CONTAMINATION MEASURE, and it is mechanical on purpose.  `:LA7` asks for a
   policy constructed from a completed item's STATEMENT AS RECORDED -- but a
   ledger row's :statement is a mutable field, and some rows were REWRITTEN after
   the work to narrate what was found.  For such a row the statement is not a
   record of what was known before the work, and a cascade that agrees with the
   resolution has retrodicted nothing.

   Measured rather than judged: the fraction of the statement's 6-word shingles
   that occur verbatim in the resolution text.  A statement written when the row
   was opened shares near-zero verbatim 6-grams with an evidence field written
   afterwards; a statement rewritten from the evidence shares many.  This lives
   in the RESOLUTIONS half because it needs both texts, so it cannot reach the
   constructor.  It is reported per item and no item is dropped for it -- the
   selection rule was fixed in advance and re-picking on a measurement taken
   afterwards is the tuning this whole design refuses."
  [statement resolution-text]
  (let [a (shingles 6 statement)
        b (shingles 6 resolution-text)
        f (fn [xs ys] (if (empty? xs)
                        {:in-statement 0 :shared 0 :fraction 0.0}
                        (let [shared (count (filter (set ys) xs))]
                          {:in-statement (count xs)
                           :shared shared
                           :fraction (/ (Math/round (* 10000.0 (/ (double shared) (count xs))))
                                        10000.0)})))]
    (sorted-map
     :verbatim-6-grams (f a b)
     ;; THE SIX-GRAM MEASURE IS WEAK AND THIS RECORDS THAT RATHER THAN HIDING IT.
     ;; On the first run it read 0.0036 for :wm/C23, whose statement plainly
     ;; narrates its own resolution -- because the rewrite RESTATES the finding
     ;; in different words.  A verbatim word sequence is the wrong carrier.  What
     ;; a post-hoc rewrite does carry over is the DISTINCTIVE TOKENS: the
     ;; file:line pointers, identifiers and numbers that only exist once the work
     ;; has been done.  Both measures are reported; neither gates anything.
     :distinctive-tokens (f (distinctive-tokens statement)
                            (distinctive-tokens resolution-text)))))

(defn- wm-resolution [row]
  (sorted-map
   :item (wm-item-id row)
   :row-id (:id row)
   :status (:status row)
   ;; :acceptance is CONTEXT, not a resolution: it was written when the row was
   ;; opened, alongside the statement.  It is here because a comparator judging
   ;; "did the cascade retrodict what was done" needs to know what the row was
   ;; asked to do; it is labelled so it is not mistaken for the outcome.
   :acceptance-written-before-the-work (:acceptance row)
   :statement-resolution-overlap
   (overlap (str (:statement row)) (str (:evidence row) " " (:review row)))
   :resolution (sorted-map :evidence (:evidence row)
                           :review (:review row)
                           :reviewed-by (:reviewed-by row)
                           :taken-by (:taken-by row))))

(defn- ticket-resolution [row]
  (let [b (ticket-body row)]
    (sorted-map
     :item (ticket-item-id row)
     :row-id (:ticket row)
     :status (:status row)
     :acceptance-written-before-the-work nil
     :statement-resolution-overlap
     (overlap (str (:title b) " " (:goal b)) (str (:status-line b) " " (:status-line row)))
     :resolution (sorted-map :status-line (:status-line b)
                             :index-status-line (:status-line row)
                             :triaged-by (:triaged-by row)))))

;; ---------------------------------------------------------------------------
;; the reports
;; ---------------------------------------------------------------------------

(defn- selection-block [wm tk]
  (sorted-map
   :rule (sorted-map
          :wm-completed-statuses (into (sorted-set) wm-completed)
          :ticket-completed-statuses (into (sorted-set) ticket-completed)
          :classes-excluded [:J]
          :min-statement-chars min-statement-chars
          :min-resolution-chars min-resolution-chars
          :spacing "indices (quot (* i n) k) for i in 0..k-1, over the qualifying list in ledger order"
          :wm-take wm-take
          :tickets-take tickets-take)
   :re-measured-not-carried-forward
   (str "The row's own denominators are as of when it was written. Re-measured on this run: "
        "wm-contract " (:completed wm) " completed of " (:rows-in-ledger wm) " rows, "
        (:qualifying wm) " qualifying; tickets " (:completed tk) " completed of "
        (:rows-in-ledger tk) " rows, " (:qualifying tk) " qualifying.")
   :wm (dissoc wm :selected)
   :tickets (dissoc tk :selected)))

(defn blind-report []
  (let [wm (wm-selected) tk (ticket-selected)]
    (sorted-map
     :schema :la7/retrodiction-items-v1
     :produced-by "checks/retrodiction_items.clj --blind"
     :contains-no-resolution?
     "TRUE BY CONSTRUCTION: the only ledger fields read into this file are :statement (wm) and the title plus ## Goal section (tickets). No :evidence, :review, :acceptance, :status, :pointers or triage line is copied here."
     :selection (selection-block wm tk)
     :items (vec (concat (map wm-blind (:selected wm))
                         (map ticket-blind (:selected tk)))))))

(defn resolutions-report []
  (let [wm (wm-selected) tk (ticket-selected)]
    (sorted-map
     :schema :la7/retrodiction-resolutions-v1
     :produced-by "checks/retrodiction_items.clj --resolutions"
     :written-after "the constructor's commit; see the ns docstring"
     :selection (selection-block wm tk)
     :items (vec (concat (map wm-resolution (:selected wm))
                         (map ticket-resolution (:selected tk)))))))

(defn- render [x] (with-out-str (pprint/pprint x)))

(defn- verify []
  (let [checks [[blind-path (render (blind-report))]
                [resolutions-path (render (resolutions-report))]]]
    (vec (for [[path expected] checks
               :let [f (io/file path)]
               :when (.exists f)
               :let [actual (slurp f)]
               :when (not= expected actual)]
           {:file path :finding :regenerates-differently
            :expected-sha256 (sha256 expected) :on-disk-sha256 (sha256 actual)}))))

(defn -main [& args]
  (let [mode (or (first args) "--blind")]
    (case mode
      "--blind"
      (let [r (blind-report)]
        (spit blind-path (render r))
        (println (format "wrote %s: %d items; wm %d/%d qualifying, tickets %d/%d qualifying"
                         blind-path (count (:items r))
                         (get-in r [:selection :wm :qualifying])
                         (get-in r [:selection :wm :completed])
                         (get-in r [:selection :tickets :qualifying])
                         (get-in r [:selection :tickets :completed])))
        (doseq [i (:items r)] (println "  " (:item i) (str (count (:statement i)) " chars"))))

      "--resolutions"
      (let [r (resolutions-report)]
        (spit resolutions-path (render r))
        (println (format "wrote %s: %d items" resolutions-path (count (:items r))))
        (doseq [i (:items r)] (println "  " (:item i) (:status i))))

      "--verify"
      (let [failures (verify)]
        (doseq [f failures] (println "FAIL" (pr-str f)))
        (println (format "retrodiction-items --verify: %d file(s) disagree with the ledgers"
                         (count failures)))
        (shutdown-agents)
        (System/exit (if (seq failures) 1 0)))

      (do (println "usage: -m retrodiction-items [--blind|--resolutions|--verify]")
          (shutdown-agents)
          (System/exit 2)))
    (shutdown-agents)
    (System/exit 0)))
