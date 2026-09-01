#!/usr/bin/env bb
;; worklist_check.bb -- prove the ledger before anyone acts on it (class 6a).
(require '[clojure.edn :as edn]
         '[clojure.string])
(def w (edn/read-string (slurp (or (first *command-line-args*)
                                    (str (.getParent (.getAbsoluteFile (java.io.File. *file*))) "/worklist.edn")))))
(defn die [& m] (binding [*out* *err*] (apply println "worklist_check:" m)) (System/exit 1))
(when-not (= :wm/worklist-v1 (:schema w)) (die "unexpected schema"))
(def ids (map :id (:items w)))
(when (not= (count ids) (count (set ids))) (die "duplicate ids"))
(doseq [i (:items w)]
  (doseq [k [:id :class :status :owner :statement :acceptance]] (when-not (contains? i k) (die (:id i) "lacks" k)))
  (when-not (contains? (:classes w) (:class i)) (die (:id i) "unknown class" (:class i)))
  (when-not (contains? (:statuses w) (:status i)) (die (:id i) "unknown status" (:status i)))
  (when (and (= :J (:class i)) (not (or (= :needs-joe (:status i)) (= :done (:status i))))) (die (:id i) "class J must be :needs-joe or :done"))
  ;; Joe, 2026-09-01: "if there's a decision to be made that isn't
  ;; predetermined by the theory, then we need to note that down as a decision.
  ;; And probably explore all the different branches ... so that we can make an
  ;; informed decision about which one performs best. But asking me to make an
  ;; advanced ruling is just not a great way to get good results because I have
  ;; nothing to go on."
  ;;
  ;; So a choice the theory does not determine is a CHOICE POINT in :choices,
  ;; with the arms named and the measurement that separates them; the branches
  ;; are BUILT AND RUN behind flags; the decision is made on the numbers and
  ;; recorded with them. Joe sees RESULTS and can veto or reopen.
  ;;
  ;; A J row is the residue only: the arms differ in machine behaviour AND no
  ;; experiment we can run would separate them. Its :bar must therefore say why
  ;; the arms CANNOT BE RUN -- not merely that the answer is unclear.
  (when (= :J (:class i))
    (when-not (map? (:bar i))
      (die (:id i) "class J needs a :bar map saying how it earns Joe's attention"
           "-- {:a \"not decidable from sources/code/a prior ruling because...\" :b \"no experiment on a recorded field distinguishes the arms because...\"},"
           "or {:retrospective \"...\"} for a row that did not meet the bar"))
    (let [bar (:bar i)]
      (when (= :needs-joe (:status i))
        (doseq [k [:a :b]]
          (when (clojure.string/blank? (str (get bar k)))
            (die (:id i) "is :needs-joe but its :bar lacks" k
                 "-- an open J row must state BOTH conditions; if it cannot, build the branches instead")))
        (when (clojure.string/blank? (str (:why-not-runnable bar)))
          (die (:id i) "is :needs-joe but its :bar lacks :why-not-runnable"
               "-- Joe's rule of 2026-09-01: a choice the theory does not settle gets BRANCHES BUILT AND RUN,"
               "not an advance ruling. Only a choice whose arms cannot be run reaches him as a question.")))))
  (when (and (#{:done-unreviewed :done} (:status i)) (not (:evidence i))) (die (:id i) "done without :evidence"))
  (when (and (= :done (:status i)) (not (:reviewed-by i))) (die (:id i) ":done without :reviewed-by")))

;; ---------------------------------------------------------------------------
;; A signature must not come to describe a later file state (claude-1's
;; proposal, 2026-09-01, after it happened twice in one session: C14 then C15).
;; A row that signed a registry entry names the entry with :covers-key and the
;; sha it read with :review-covers. If that entry's value in the file today
;; differs from its value at that sha, the signature is on something else and
;; the row is NOT reviewed.
;;
;; Scoped by KEY, not by file: any later commit touching aif-equations.edn
;; would otherwise invalidate every prior signature, which would make the
;; ledger unusable and teach everyone to ignore the check.
;;
;; Rows without :covers-key are not checked, and the count of unchecked rows is
;; printed rather than passed over -- an absence should be visible, not implied.

(require '[clojure.java.shell :as shell]
         '[clojure.string :as str])

(require '[clojure.java.io :as io])
;; Resolve every path against the REPO ROOT, found from this script's own
;; location -- not from the caller's cwd. wm-edge-loop.sh calls this script by
;; absolute path from wherever the loop was started; run from /tmp the old
;; relative slurp and cwd-bound `git show` reported "the signed sha is not in
;; this history", a cwd problem wearing a history problem's message (C16 review).
(def script-dir (let [f (io/file *file*)] (.getParentFile (.getAbsoluteFile f))))
(def repo-root (let [{:keys [exit out]} (shell/sh "git" "rev-parse" "--show-toplevel" :dir script-dir)]
                 (when (zero? exit) (str/trim out))))
(when-not repo-root (die "cannot find the git repo root from" (str script-dir)))

(defn- resolve-path
  "get-in, except that a MAP element selects by content instead of position.

   `[:equations {:id :precision}]` finds the equation whose :id is :precision;
   `[:holes {:edge [:R6 :R16]}]` finds that hole. Index addressing into
   :equations and :holes is a false-positive generator: inserting a row -- as
   C8 did with :dirichlet-accumulation -- shifts every later index, and the
   check would then report content drift that is really position drift, a
   stale signature that is not stale (claude-1, C17 review). Returns ::absent
   when any step misses, which compares unequal to a real value and equal to
   another absence -- so a key deleted on both sides is not silently a match
   with something else."
  [m key-path]
  (reduce (fn [acc step]
            (cond
              (= ::absent acc) (reduced ::absent)
              (map? step) (or (first (filter #(= step (select-keys % (keys step)))
                                             acc))
                              (reduced ::absent))
              :else (get acc step ::absent)))
          m
          key-path))

(defn- key-paths
  "One key-path, or several. A vector of vectors is several; anything else is
   one. C4, C6, C7 and C8 each signed more than one registry entry, and giving
   such a row a single key-path would check one entry while implying coverage
   of all of them."
  [covers-key]
  (if (vector? (first covers-key)) covers-key [covers-key]))

(defn- registry-at
  "The registry map as of SHA, or nil when the file did not exist there."
  [sha path]
  (let [{:keys [exit out]} (shell/sh "git" "show" (str sha ":" path) :dir repo-root)]
    (when (zero? exit) (edn/read-string out))))

(def superseded-rows
  ;; TN 9a: an entry changed after signature gets a NEW row naming the row it
  ;; supersedes, and the old signature is not touched. So a superseded row's
  ;; signature is expected to be stale -- that is the point of it -- and the
  ;; check skips it. The superseding row must exist, or "superseded" is just a
  ;; word that switches the check off.
  (filter #(and (= :done (:status %)) (:superseded-by %)) (:items w)))

(doseq [i superseded-rows]
  (when-not (some #(= (:superseded-by i) (:id %)) (:items w))
    (die (:id i) "names" (:superseded-by i) "as superseding it, and no such row exists")))

(def signed-registry-rows
  (filter #(and (= :done (:status %)) (vector? (:covers-key %))
                (not (:superseded-by %)))
          (:items w)))

;; `:covers-key :none` is a DECLARATION that the row touched no registry entry
;; (C16 covers a script; a report row covers a .md). It keeps the unchecked
;; count honest without hiding anything -- but it must not become the way a row
;; opts out of the check, so a row claiming :none may not also name a registry
;; path (claude-1's convention, C16 review).
(doseq [i (:items w)]
  (when (and (= :none (:covers-key i)) (:registry-path i))
    (die (:id i) "declares :covers-key :none and also names a :registry-path"
         "-- one of them is wrong; :none means the row touched no registry entry")))

(def unchecked-signed-rows
  ;; Deliberately a SUPERSET: every signed C row without a :covers-key, not
  ;; only those that declare a :registry-path. Filtering on :registry-path
  ;; would report zero unchecked rows while C rows that did touch the registry
  ;; sat unchecked -- an absence reported as a success, which is the defect
  ;; class this ledger exists to catch.
  (filter #(and (= :done (:status %)) (= :C (:class %)) (not (:covers-key %)))
          (:items w)))

(def declared-no-registry-rows
  (filter #(and (= :done (:status %)) (= :none (:covers-key %))) (:items w)))

(doseq [i signed-registry-rows]
  (let [sha (:review-covers i)
        path (or (:registry-path i) "holes/labs/wm-contract/aif-equations.edn")
        key-path (:covers-key i)]
    (when-not sha
      (die (:id i) "has :covers-key but no :review-covers -- the signature names no sha"))
    (let [then (registry-at sha path)]
      (when-not then
        (die (:id i) "cannot read" path "at" sha "-- the signed sha is not in this history"))
      (let [now-file (edn/read-string (slurp (io/file repo-root path)))]
        (doseq [kp (key-paths key-path)]
          (let [was (resolve-path then kp)
                now (resolve-path now-file kp)]
            (when (= ::absent was)
              (die (:id i) "covers" (pr-str kp) "which did not exist at" sha
                   "-- the signature names an entry that was not there to sign"))
            (when (not= was now)
              (die (:id i) "signature is stale:" (pr-str kp) "in" path
                   "changed after" sha "was signed by" (:reviewed-by i)
                   "-- set the row back to :done-unreviewed, or open a new row that supersedes it (TN 9a)"))))))))


;; A read is against a COMMIT, never against the working tree (claude-1, TN 9a,
;; 2026-09-01). The shared checkout means one seat's in-progress edit is visible
;; to the other's slurp: a reader can sign content that is in motion, and a
;; `git add` of the ledger can sweep the other seat's half-finished work into
;; the signer's commit. The stale-signature check only catches this when the
;; covered key happens to have moved already. So: warn, loudly and by name,
;; whenever the ledger directory is dirty. Not fatal -- ordinary editing leaves
;; it dirty all the time -- but nobody should sign while this line is printed.
(let [{:keys [exit out]} (apply shell/sh
                                ["git" "status" "--porcelain" "--"
                                 (str script-dir) :dir repo-root])
      dirty (when (zero? exit) (remove clojure.string/blank? (str/split-lines out)))]
  (when (seq dirty)
    (println "worklist_check: WARNING -- ledger directory has uncommitted changes;"
             "a signature taken now would cover content in motion. Do not sign until clean:")
    (doseq [d dirty] (println "worklist_check:   " (str/trim d)))))


;; A row id must never look like a control-map node. The nodes are R1..R20
;; (R2 = observe, R7 = precision, R14 = temperature); the run rows are
;; RUN1.. . Until 2026-09-01 both were R<digit> and "R2 traverses R2->R7" was a
;; sentence you had to parse twice. Reject the collision rather than trusting
;; everyone to remember it.
(doseq [i (:items w)]
  (when (re-matches #"R\d+[a-z]?" (name (:id i)))
    (die (:id i) "looks like a control-map node (R1..R20). Run rows are RUN1.., "
         "so a bare R<digit> always means a node.")))

(def by-status (frequencies (map :status (:items w))))
(println (format "worklist_check: %d items OK; %s; %d signed registry entries verified unchanged since signature, %d superseded and skipped, %d declared :covers-key :none, %d signed registry rows carry no :covers-key and are NOT checked"
                 (count (:items w)) (pr-str by-status)
                 (count signed-registry-rows) (count superseded-rows)
                 (count declared-no-registry-rows)
                 (count unchecked-signed-rows)))
