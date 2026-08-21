(ns futon3.inbox-zero.watcher
  "Git observation and explicit witness intake for multi_watcher.

  Git state can create file observations only. Session-file claims enter solely
  through the typed witness stream, so filesystem dirt never becomes guessed
  authorship."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as state])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
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
  [{:keys [state-path witness-path roots now]
    :or {now (Date.)}}]
  (when-not (and (string? state-path) (not (str/blank? state-path)))
    (throw (ex-info "Inbox-zero state path is required"
                    {:error/type :inbox-zero/state-path-required})))
  (let [with-witnesses (state/append-records! state-path (read-witnesses witness-path))
        observations (mapcat #(observe-repo with-witnesses % now) roots)
        stored (state/append-records! state-path observations)]
    {:state stored
     :observations-written (count observations)
     :projection (projection/project-dirty-sets stored now)}))
