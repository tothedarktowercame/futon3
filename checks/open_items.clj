(ns open-items
  "The OPEN-ITEM EXTRACTOR of worklist row `:LA8`
   (futon3/holes/labs/library-contract/worklist.edn) -- the CONSTRUCTION class
   of P-validated-R5.md:697-702 (Joe's fifth exchange): run through real work
   items \"not to work on them, but only to see whether policies could be
   constructed that would plausibly allow working on them\".

   WHAT THIS FILE IS AND IS NOT.  `:LA7` needed a SPLIT -- statements to the
   constructor, resolutions withheld -- because a completed row records what was
   done and a constructor allowed to read it has retrodicted nothing.  An OPEN
   item has no resolution, so there is nothing to withhold and no blindness
   discipline to run.  That is a genuine weakening of the evidence and is stated
   here rather than dressed up: `:LA7` could be wrong in a way git history would
   catch; `:LA8` cannot be wrong in that way because there is no recorded answer
   for a cascade to agree with.  What replaces it is the acceptance's own
   discipline -- a plausibility judgement made by someone who did not construct,
   persisted beside the cascade -- and the two measures below, which say what a
   reader should discount before reading a judgement.

   THE TWO MEASURES, and why an open item needs measuring at all.

   (1) REMEDY LANGUAGE ALREADY IN THE STATEMENT.  A futon3c ticket's triage line
   often names candidate fixes, and a worklist row's statement often names the
   build it wants.  A cascade that agrees with a remedy the statement already
   carries has proposed nothing.  This is the open-item analogue of `:LA7`'s
   post-hoc narration and is measured the same way: mechanically, over a marker
   list fixed in this file, reported per item, gating nothing.  The markers are
   TEMPLATE STRINGS -- section headings of the ticket template and the fixed
   prefix of the triage line -- not phrases chosen after reading any item.  They
   are counted over the ADDRESSED SPAN and, separately, over the WHOLE record, so
   a reader can see what the addressing rule left out rather than take on trust
   that it left out nothing.

   (2) THE CORPUS IS RE-MEASURED, NOT CARRIED FORWARD.  Row `:LA8` states its
   denominators as \"4 :open + 2 :blocked in wm-contract; 7 :still-open + 3 :open
   in tickets-index.edn\".  Re-measured on every run, and on 2026-09-02 the first
   half is already stale: wm-contract has 0 :open and 2 :blocked -- the four open
   rows were completed after the row was written.  The wm half of this corpus is
   therefore two blocked rows, and `:selection` says so on every run.

   THE ADDRESS a cue must occur in, per decisions.edn :cue-licence-for-a-ledger-row:
     - a worklist row -> the EDN key path [file, row-id, :statement];
     - a ticket -> the line span [1, END] where END is the last line before the
       SECOND `## ` heading (or EOF if there is none).  Title, triage line and
       the first section, verbatim and contiguous, so the span re-reads exactly.
   The addressing rule is uniform and takes no ticket's headings by name; the
   open tickets have no `## Goal` section, which is what `:LA7`'s ticket address
   used, so that address does not transfer and this one replaces it.

     --open    -> checks/open-items.edn
     --verify  -> re-derive and compare against what is on disk"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [java.security MessageDigest]))

(def wm-ledger "../futon2/holes/labs/wm-contract/worklist.edn")
(def tickets-index "../futon3c/holes/tickets/tickets-index.edn")
(def tickets-dir "../futon3c/holes/tickets")

(def open-path "checks/open-items.edn")

;; ---------------------------------------------------------------------------
;; the selection rule -- fixed here, applied by code, and small enough to check
;; ---------------------------------------------------------------------------

(def wm-open
  "The row names \":open + :blocked\".  Both are kept even though :open is now
   empty, so the rule does not quietly become \"the blocked ones\"."
  #{:open :blocked})

(def ticket-open
  "The row names \":still-open + :open\".  tickets-index.edn also carries
   :partial, :scoped, :watch, :parked and :blocked; those are NOT open by the
   row's own definition and are not silently folded in to enlarge the corpus."
  #{:still-open :open})

(def min-statement-chars
  "As `:LA7`: below this a statement cannot carry a Tension with more than one
   clause, and `antecedent-holds?` needs an IF-hit and a HOWEVER-hit to fire."
  200)

(def wm-take 2)
(def tickets-take 3)

(defn evenly-spaced
  "Indices `(floor (i*n/k))` for i in 0..k-1 -- k items spread across the whole
   qualifying list in ledger order, so the packet is not the first k rows."
  [n k]
  (if (or (zero? n) (zero? k))
    []
    (vec (distinct (for [i (range (min k n))] (quot (* i n) k))))))

(def remedy-markers
  "TEMPLATE STRINGS, not phrases picked after reading an item: the fixed prefix
   of the triage line written by the 2026-09-01 triage pass, the fixed
   `Candidate fixes` lead-in it uses, and five `## ` headings that occur as
   section names across the ticket set.  Case-insensitive substring counts."
  ["**status (triaged"
   "candidate fixes"
   "## what would close this"
   "## the repair"
   "## acceptance"
   "## disposition"
   "## hardening ladder"])

(defn sha256 [^String s]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest md (.getBytes s "UTF-8"))))))

(defn- remedy-count
  "How many of the fixed markers occur in `text`, and which."
  [text]
  (let [lc (str/lower-case (str text))
        hits (filterv #(str/includes? lc %) remedy-markers)]
    (sorted-map :markers-present (vec hits) :count (count hits))))

(defn- remedy-measure [span whole]
  (sorted-map
   :in-the-addressed-span (remedy-count span)
   :in-the-whole-record (remedy-count whole)
   :note "A cascade that agrees with a remedy the statement already carries has proposed nothing. Reported per item; nothing is dropped for it."))

;; ---------------------------------------------------------------------------
;; wm-contract rows
;; ---------------------------------------------------------------------------

(defn- wm-rows [] (:items (edn/read-string (slurp wm-ledger))))

(defn- wm-qualifies? [row]
  (and (contains? wm-open (:status row))
       (not= :J (:class row))
       (>= (count (str (:statement row))) min-statement-chars)))

(defn- wm-selected []
  (let [all (wm-rows)
        open (filterv #(contains? wm-open (:status %)) all)
        qualifying (filterv wm-qualifies? all)
        idxs (evenly-spaced (count qualifying) wm-take)]
    {:ledger wm-ledger
     :rows-in-ledger (count all)
     :status-histogram (into (sorted-map) (frequencies (map :status all)))
     :open (count open)
     :open-ids (mapv (comp name :id) open)
     :qualifying (count qualifying)
     :qualifying-ids (mapv (comp name :id) qualifying)
     :indices idxs
     :selected (mapv #(nth qualifying %) idxs)}))

(defn- wm-item [row]
  (let [stmt (str (:statement row))]
    (sorted-map
     :item (keyword "wm" (name (:id row)))
     :ledger wm-ledger
     :row-id (:id row)
     :status (:status row)
     :statement stmt
     :statement-sha256 (sha256 stmt)
     :statement-address (sorted-map :kind :edn-key-path
                                    :file wm-ledger
                                    :row-id (:id row)
                                    :key :statement)
     :remedy-language
     (remedy-measure stmt (str stmt " " (:acceptance row))))))

;; ---------------------------------------------------------------------------
;; futon3c tickets
;; ---------------------------------------------------------------------------

(defn- first-section-end
  "1-indexed last line of the span [1, END]: the line before the SECOND `## `
   heading, or the last line of the file when there are fewer than two.  Uniform
   over the ticket set and names no heading, so no ticket's structure is
   privileged and no section is selected by what it says."
  [lines]
  (let [hs (vec (keep-indexed (fn [i l] (when (str/starts-with? l "## ") i)) lines))]
    (if (>= (count hs) 2) (nth hs 1) (count lines))))

(defn- ticket-body [row]
  (let [f (io/file tickets-dir (:ticket row))]
    (when (.exists f)
      (let [raw (slurp f)
            lines (vec (str/split-lines raw))
            end (first-section-end lines)]
        {:file (str tickets-dir "/" (:ticket row))
         :raw raw
         :end end
         :statement (str/join "\n" (subvec lines 0 end))}))))

(defn- ticket-qualifies? [row]
  (let [b (ticket-body row)]
    (and (contains? ticket-open (:status row))
         (some? b)
         (>= (count (:statement b)) min-statement-chars))))

(defn- ticket-selected []
  (let [all (edn/read-string (slurp tickets-index))
        open (filterv #(contains? ticket-open (:status %)) all)
        qualifying (filterv ticket-qualifies? all)
        idxs (evenly-spaced (count qualifying) tickets-take)]
    {:ledger tickets-index
     :rows-in-ledger (count all)
     :status-histogram (into (sorted-map) (frequencies (map :status all)))
     :open (count open)
     :open-ids (mapv :ticket open)
     :qualifying (count qualifying)
     :qualifying-ids (mapv :ticket qualifying)
     :indices idxs
     :selected (mapv #(nth qualifying %) idxs)}))

(defn- ticket-item [row]
  (let [b (ticket-body row)
        stmt (:statement b)]
    (sorted-map
     :item (keyword "ticket" (-> (:ticket row) (str/replace #"^T-" "") (str/replace #"\.md$" "")))
     :ledger tickets-index
     :row-id (:ticket row)
     :status (:status row)
     :statement stmt
     :statement-sha256 (sha256 stmt)
     :statement-address (sorted-map :kind :line-span
                                    :file (:file b)
                                    :lines [1 (:end b)])
     :remedy-language (remedy-measure stmt (:raw b)))))

;; ---------------------------------------------------------------------------
;; the report
;; ---------------------------------------------------------------------------

(defn- selection-block [wm tk]
  (sorted-map
   :rule (sorted-map
          :wm-open-statuses (into (sorted-set) wm-open)
          :ticket-open-statuses (into (sorted-set) ticket-open)
          :classes-excluded [:J]
          :min-statement-chars min-statement-chars
          :spacing "indices (quot (* i n) k) for i in 0..k-1, over the qualifying list in ledger order"
          :wm-take wm-take
          :tickets-take tickets-take
          :ticket-address "title line through the last line before the SECOND `## ` heading; no heading is named")
   :re-measured-not-carried-forward
   (str "Row :LA8 states \"4 :open + 2 :blocked in wm-contract; 7 :still-open + 3 :open in "
        "tickets-index.edn\" as of when it was written. Re-measured on this run: wm-contract "
        (:open wm) " open-or-blocked of " (:rows-in-ledger wm) " rows (" (:qualifying wm)
        " qualifying); tickets " (:open tk) " open of " (:rows-in-ledger tk) " rows ("
        (:qualifying tk) " qualifying).")
   :wm (dissoc wm :selected)
   :tickets (dissoc tk :selected)))

(defn open-report []
  (let [wm (wm-selected) tk (ticket-selected)]
    (sorted-map
     :schema :la8/open-items-v1
     :produced-by "checks/open_items.clj --open"
     :there-is-no-withheld-half
     "By construction, and this is the row's weakness rather than a saving. An open item records no resolution, so there is no blind/resolutions split to run and no git-history check on the constructor. What the acceptance substitutes is a plausibility judgement by someone who did not construct, persisted beside the cascade."
     :selection (selection-block wm tk)
     :items (vec (concat (map wm-item (:selected wm))
                         (map ticket-item (:selected tk)))))))

(defn- render [x] (with-out-str (pprint/pprint x)))

(defn- verify []
  (let [f (io/file open-path)]
    (if-not (.exists f)
      [{:file open-path :finding :missing}]
      (let [expected (render (open-report)) actual (slurp f)]
        (if (= expected actual)
          []
          [{:file open-path :finding :regenerates-differently
            :expected-sha256 (sha256 expected) :on-disk-sha256 (sha256 actual)}])))))

(defn -main [& args]
  (let [mode (or (first args) "--open")]
    (case mode
      "--open"
      (let [r (open-report)]
        (spit open-path (render r))
        (println (format "wrote %s: %d items; wm %d/%d qualifying, tickets %d/%d qualifying"
                         open-path (count (:items r))
                         (get-in r [:selection :wm :qualifying])
                         (get-in r [:selection :wm :open])
                         (get-in r [:selection :tickets :qualifying])
                         (get-in r [:selection :tickets :open])))
        (doseq [i (:items r)]
          (println "  " (:item i) (:status i) (str (count (:statement i)) " chars")
                   "remedy-markers span" (get-in i [:remedy-language :in-the-addressed-span :count])
                   "whole" (get-in i [:remedy-language :in-the-whole-record :count]))))

      "--verify"
      (let [failures (verify)]
        (doseq [f failures] (println "FAIL" (pr-str f)))
        (println (format "open-items --verify: %d file(s) disagree with the ledgers" (count failures)))
        (shutdown-agents)
        (System/exit (if (seq failures) 1 0)))

      (do (println "usage: -m open-items [--open|--verify]")
          (shutdown-agents)
          (System/exit 2)))
    (shutdown-agents)
    (System/exit 0)))
