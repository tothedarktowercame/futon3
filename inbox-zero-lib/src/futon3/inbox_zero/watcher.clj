(ns futon3.inbox-zero.watcher
  "Git observation and explicit witness intake for multi_watcher.

  Git state can create file observations only. Session-file claims enter solely
  through the typed witness stream, so filesystem dirt never becomes guessed
  authorship."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as state])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.time OffsetDateTime]
           [java.util Date]))

(defn- sha-256-bytes [bytes]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (str "sha256:"
         (apply str (map #(format "%02x" (bit-and % 0xff)) digest)))))

(defn- sha-256-value [value]
  (sha-256-bytes (.getBytes (pr-str value) StandardCharsets/UTF_8)))

(defn- git!
  [repo-root & args]
  (let [{:keys [exit out err]} (apply sh "git" "-C" repo-root args)]
    (when-not (zero? exit)
      (throw (ex-info "Git command failed"
                      {:error/type :inbox-zero/git-failed
                       :repo/root repo-root :args (vec args)
                       :exit exit :stderr err})))
    out))

(defn worktree-id [repo-root]
  (let [canonical (.getCanonicalPath (io/file repo-root))]
    (str "worktree:" (subs (sha-256-value canonical) 7 23))))

(defn- cursor-id [worktree sha reason prior-id]
  (str "commit-scan-cursor:"
       (subs (sha-256-value [worktree sha reason prior-id]) 7)))

(defn- cursor-record [worktree sha reason prior-id observed-at]
  {:record/type :inbox-zero/commit-scan-cursor
   :cursor/id (cursor-id worktree sha reason prior-id)
   :worktree/id worktree
   :cursor/sha sha
   :cursor/reason reason
   :prior/cursor-id prior-id
   :observed-at observed-at})

(defn- git-lines [repo-root & args]
  (->> (apply git! repo-root args)
       str/split-lines
       (remove str/blank?)
       vec))

(defn- reachable? [repo-root ancestor descendant]
  (zero? (:exit (sh "git" "-C" repo-root "merge-base" "--is-ancestor"
                    ancestor descendant))))

(defn- first-run-baseline [repo-root head lookback]
  (let [history (git-lines repo-root "rev-list" "--reverse" head)
        index (max 0 (- (count history) 1 lookback))]
    (nth history index)))

(defn- commit-observation [repo-root repo-id worktree sha observed-at]
  (let [[commit parents authored]
        (str/split (str/trim (git! repo-root "show" "-s"
                                  "--format=%H%x00%P%x00%aI" sha)) #"\u0000" -1)
        paths (->> (git-lines repo-root "diff-tree" "--root" "--no-commit-id"
                              "--name-only" "-r" sha)
                   sort vec)]
    {:record/type :inbox-zero/commit-observation
     :commit-observation/id
     (str "commit-observation:" (subs (sha-256-value [worktree commit]) 7))
     :repo/id repo-id
     :worktree/id worktree
     :commit/sha commit
     :change/id nil
     :parents (if (str/blank? parents) [] (vec (str/split parents #" ")))
     :paths paths
     :authored-at (Date/from (.toInstant (OffsetDateTime/parse authored)))
     :observed-at observed-at
     :source :git}))

(defn observe-commits
  "Return immutable cursor and commit records for one repository.

  The live scanner is forward-only because commit observations link witnessed
  claims, and claims cannot predate producer activation. On first run it
  records HEAD as a baseline and emits no observations. `commit-lookback'
  explicitly opts into a bounded backfill by baselining at HEAD~N (clamped at
  the root); retroactive mission claiming remains a separate pass. History
  rewrites produce a loud :rebaseline-rewrite cursor and no guessed links."
  [store {:keys [path label commit-lookback] :or {commit-lookback 0}} observed-at]
  (when-not (and (integer? commit-lookback) (not (neg? commit-lookback)))
    (throw (ex-info "Commit lookback must be a non-negative integer"
                    {:error/type :inbox-zero/invalid-commit-lookback
                     :commit-lookback commit-lookback})))
  (let [repo-root (.getCanonicalPath (io/file path))
        worktree (worktree-id repo-root)
        head (str/trim (git! repo-root "rev-parse" "HEAD"))
        current (get (projection/current-commit-cursors store) worktree)]
    (cond
      (nil? current)
      (let [baseline (first-run-baseline repo-root head commit-lookback)
            first-cursor (cursor-record worktree baseline :baseline nil observed-at)
            shas (git-lines repo-root "rev-list" "--reverse"
                            (str baseline ".." head))
            observations (mapv #(commit-observation repo-root label worktree % observed-at)
                               shas)]
        (cond-> [first-cursor]
          (seq shas) (into observations)
          (seq shas) (conj (cursor-record worktree head :advance
                                          (:cursor/id first-cursor) observed-at))))

      (not (reachable? repo-root (:cursor/sha current) head))
      [(cursor-record worktree head :rebaseline-rewrite
                      (:cursor/id current) observed-at)]

      (= (:cursor/sha current) head) []

      :else
      (let [shas (git-lines repo-root "rev-list" "--reverse"
                            (str (:cursor/sha current) ".." head))]
        (into (mapv #(commit-observation repo-root label worktree % observed-at) shas)
              [(cursor-record worktree head :advance
                              (:cursor/id current) observed-at)])))))

(defn- porcelain-status [xy]
  (cond
    (str/includes? xy "R") :renamed
    (str/includes? xy "D") :deleted
    (= xy "??") :untracked
    :else :modified))

(defn parse-porcelain-v1-z
  "Parse `git status --porcelain=v1 -z` into current dirty path records."
  [output]
  (let [tokens (str/split output #"\u0000" -1)]
    (loop [remaining tokens result []]
      (let [entry (first remaining)]
        (if (str/blank? entry)
          result
          (let [xy (subs entry 0 2)
                path (subs entry 3)
                rename? (or (str/includes? xy "R") (str/includes? xy "C"))
                ;; Under -z, the destination is in ENTRY and the following
                ;; token is the source path. Only the destination is current.
                remaining* (if rename? (nnext remaining) (next remaining))]
            (recur remaining*
                   (conj result {:path path :git/status (porcelain-status xy)}))))))))

(defn git-dirty-paths [repo-root]
  (parse-porcelain-v1-z
   (git! repo-root "status" "--porcelain=v1" "-z" "--untracked-files=all")))

(defn- content-hash [repo-root path git-status]
  (when-not (= :deleted git-status)
    (let [file (io/file repo-root path)]
      (when (and (.exists file) (.isFile file))
        (sha-256-bytes (java.nio.file.Files/readAllBytes (.toPath file)))))))

(defn- observation-tuple [observation]
  (select-keys observation [:git/status :content/hash :head/sha :index/hash]))

(defn observe-repo
  "Return new file-observation records for one repository.

  Previously dirty paths absent from the current porcelain output receive a
  :clean transition. Unchanged state emits nothing, making repeated scans
  idempotent without relying on scan timestamps."
  [store {:keys [path label]} observed-at]
  (let [repo-root (.getCanonicalPath (io/file path))
        worktree (worktree-id repo-root)
        head (str/trim (git! repo-root "rev-parse" "HEAD"))
        current (state/records-of-type store :inbox-zero/file-observation)
        previous-by-path (->> current
                              (filter #(= worktree (:worktree/id %)))
                              (group-by :path))
        previous-by-path (update-vals
                          previous-by-path
                          #(last (sort-by (juxt :observed-at state/record-id) %)))
        dirty (git-dirty-paths repo-root)
        dirty-by-path (into {} (map (juxt :path identity)) dirty)
        formerly-dirty (->> previous-by-path
                            (keep (fn [[p observation]]
                                    (when (and (projection/dirty-statuses (:git/status observation))
                                               (not (contains? dirty-by-path p)))
                                      p))))
        candidates (concat dirty
                           (map (fn [p] {:path p :git/status :clean}) formerly-dirty))]
    (->> candidates
         (keep
          (fn [{:keys [path git/status]}]
            (let [previous (get previous-by-path path)
                  base {:record/type :inbox-zero/file-observation
                        :repo/id label
                        :repo/root repo-root
                        :worktree/id worktree
                        :path path
                        :git/status status
                        :content/hash (content-hash repo-root path status)
                        :head/sha head
                        :index/hash nil
                        :observed-at observed-at
                        :source :multi-watcher}
                  transition [worktree path (:observation/id previous)
                              (observation-tuple base)]]
              (when-not (= (observation-tuple previous) (observation-tuple base))
                (assoc base :observation/id
                       (str "file-observation:" (subs (sha-256-value transition) 7)))))))
         (sort-by :path)
         vec)))

(defn write-witness!
  "Atomically publish one immutable seat or claim record into WITNESS-DIR.
  Unique files make the intake safe for multiple independent producers."
  [witness-dir record]
  (state/validate-record record)
  (let [id (state/record-id record)
        directory (.toPath (io/file witness-dir))
        attrs (make-array FileAttribute 0)]
    (Files/createDirectories directory attrs)
    (let [target (.resolve directory (str (subs (sha-256-value id) 7) ".edn"))]
      (if (.exists (.toFile target))
        (let [existing (edn/read-string (slurp (.toFile target)))]
          (when-not (= existing record)
            (throw (ex-info "Witness id already has different content"
                            {:error/type :inbox-zero/witness-id-conflict
                             :record/id id})))
          target)
        (let [tmp (Files/createTempFile directory ".witness-" ".edn" attrs)]
          (try
            (spit (.toFile tmp) (str (pr-str record) "\n"))
            (Files/move tmp target
                        (into-array StandardCopyOption
                                    [StandardCopyOption/ATOMIC_MOVE]))
            target
            (finally
              (Files/deleteIfExists tmp))))))))

(defn read-witnesses
  "Read immutable `*.edn` witness records from DIRECTORY in stable order.
  Missing input means no witnesses; malformed records fail closed."
  [directory]
  (let [dir (when directory (io/file directory))]
    (if-not (and dir (.exists dir))
      []
      (do
        (when-not (.isDirectory dir)
          (throw (ex-info "Inbox-zero witness path must be a directory"
                          {:error/type :inbox-zero/witness-path-not-directory
                           :path (str directory)})))
        (let [type-order {:inbox-zero/session-seat 0
                          :inbox-zero/session-file-claim 1}]
          (->> (.listFiles dir)
               (filter #(and (.isFile %) (str/ends-with? (.getName %) ".edn")))
               (sort-by #(.getName %))
               (map #(edn/read-string (slurp %)))
               (sort-by (juxt #(get type-order (:record/type %) 99)
                              state/record-id))
               vec))))))

(defn run-cycle!
  "Ingest explicit witnesses, observe all configured repos, and persist once.
  Returns the resulting state and pure projection."
  [{:keys [state-path witness-path roots now commit-lookback]
    :or {now (Date.)}}]
  (when-not (and (string? state-path) (not (str/blank? state-path)))
    (throw (ex-info "Inbox-zero state path is required"
                    {:error/type :inbox-zero/state-path-required})))
  (let [with-witnesses (state/append-records! state-path (read-witnesses witness-path))
        commit-records
        (loop [stored with-witnesses roots roots records []]
          (if-let [root (first roots)]
            (let [batch (observe-commits stored
                                         (cond-> root
                                           (some? commit-lookback)
                                           (assoc :commit-lookback commit-lookback))
                                         now)]
              (recur (reduce state/apply-record stored batch)
                     (next roots) (into records batch)))
            records))
        with-commits (reduce state/apply-record with-witnesses commit-records)
        commit-observations (filterv #(= :inbox-zero/commit-observation
                                         (:record/type %))
                                     commit-records)
        links (projection/derive-session-commit-links
               with-commits commit-observations now)
        with-links (state/append-records! state-path (into (vec commit-records) links))
        observations (mapcat #(observe-repo with-links % now) roots)
        stored (state/append-records! state-path observations)]
    {:state stored
     :observations-written (count observations)
     :commit-observations-written (count commit-observations)
     :session-commit-links-written (count links)
     :projection (projection/project-dirty-sets stored now)}))

(defn send-eligible-followups!
  "POST eligible dirty sets as typed followups. Agency performs exact-session
  validation and durable dedupe; failures are returned, never treated as sent."
  [{:keys [url store projection now] :or {now (Date.)}}]
  (vec
   (for [dirty-set (:dirty-sets projection)
         :let [eligible (projection/eligibility dirty-set now)]
         :when eligible
         :let [seat (get-in store [:records (:seat/id dirty-set)])
               body {:agent (:agent/id seat) :session (:session/id seat)
                     :type "inbox-zero" :dedupe-key (:dedupe/key eligible)
                     :prompt (format "Inbox zero: %s has %d uncommitted files associated with this session. Review, commit, or explicitly release them."
                                     (:repo/id dirty-set) (:count dirty-set))
                     :metadata {:trigger (:trigger eligible)
                                :repo-id (:repo/id dirty-set)
                                :worktree-id (:worktree/id dirty-set)}}
               response (http/post url {:headers {"Content-Type" "application/json"}
                                        :body (json/generate-string body)
                                        :throw false})]]
     {:seat/id (:seat/id dirty-set) :status (:status response)
      :ok? (= 200 (:status response)) :body (:body response)})))
