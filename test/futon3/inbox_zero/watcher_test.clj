(ns futon3.inbox-zero.watcher-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as state]
            [futon3.inbox-zero.watcher :as watcher]))

(defn temp-dir [prefix]
  (.toFile (java.nio.file.Files/createTempDirectory
            prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn git! [repo & args]
  (let [{:keys [exit err] :as result} (apply sh "git" "-C" (.getPath repo) args)]
    (when-not (zero? exit)
      (throw (ex-info "test git failed" {:stderr err :args args})))
    result))

(defn init-repo []
  (let [repo (temp-dir "inbox-zero-git-test")]
    (git! repo "init" "-q")
    (git! repo "config" "user.name" "Inbox Zero Test")
    (git! repo "config" "user.email" "inbox-zero@example.invalid")
    (spit (io/file repo "tracked.txt") "base\n")
    (git! repo "add" "tracked.txt")
    (git! repo "commit" "-q" "-m" "base")
    repo))

(defn seat-record []
  {:record/type :inbox-zero/session-seat
   :seat/id "seat:codex-11:session-1"
   :agent/id "codex-11"
   :session/id "session-1"
   :surface :emacs-repl
   :host/id "dionysus"
   :workspace/root "/home/joe/code"
   :observed-at (java.util.Date. 1000)
   :registry-witness {:endpoint "/api/alpha/agents/codex-11"
                      :session/id "session-1"
                      :observed-at (java.util.Date. 1000)}})

(defn claim-record [worktree path]
  {:record/type :inbox-zero/session-file-claim
   :claim/id "claim:tool-call-1"
   :seat/id "seat:codex-11:session-1"
   :repo/id "fixture"
   :worktree/id worktree
   :path path
   :relation :edited-by
   :witness/type :tool-edit
   :witness/id "tool-call:1"
   :first-observed-at (java.util.Date. 1000)
   :last-observed-at (java.util.Date. 1000)
   :state :active})

(deftest parses-porcelain-records-including-rename-source-token
  (is (= [{:path "a.txt" :git/status :modified}
          {:path "new.txt" :git/status :renamed}
          {:path "new-file.txt" :git/status :untracked}]
         (watcher/parse-porcelain-v1-z
          " M a.txt\u0000R  new.txt\u0000old.txt\u0000?? new-file.txt\u0000"))))

(deftest git-observation-is-transition-based-and-emits-clean
  (let [repo (init-repo)
        root {:path (.getPath repo) :label "fixture"}
        state-path (.getPath (io/file repo ".git" "inbox-zero.edn"))]
    (spit (io/file repo "tracked.txt") "changed\n")
    (let [first-records (watcher/observe-repo (state/empty-state) root
                                                (java.util.Date. 2000))
          stored (state/append-records! state-path first-records)]
      (is (= ["tracked.txt"] (mapv :path first-records)))
      (is (= :modified (:git/status (first first-records))))
      (is (string? (:content/hash (first first-records))))
      (is (empty? (watcher/observe-repo stored root (java.util.Date. 3000)))
          "an unchanged rescan emits no timestamp-only record")
      (git! repo "checkout" "--" "tracked.txt")
      (let [clean-records (watcher/observe-repo stored root (java.util.Date. 4000))]
        (is (= :clean (:git/status (first clean-records))))
        (is (= "tracked.txt" (:path (first clean-records))))))))

(deftest cycle-ingests-explicit-witness-and-restarts-idempotently
  (let [repo (init-repo)
        root {:path (.getPath repo) :label "fixture"}
        worktree (watcher/worktree-id (.getPath repo))
        state-path (.getPath (io/file repo ".git" "inbox-zero.edn"))
        witness-path (.getPath (io/file repo ".git" "inbox-zero-witnesses"))]
    (spit (io/file repo "tracked.txt") "changed\n")
    (watcher/write-witness! witness-path (seat-record))
    (watcher/write-witness! witness-path (claim-record worktree "tracked.txt"))
    (let [first-cycle (watcher/run-cycle!
                       {:state-path state-path :witness-path witness-path
                        :roots [root] :now (java.util.Date. 2000)})
          second-cycle (watcher/run-cycle!
                        {:state-path state-path :witness-path witness-path
                         :roots [root] :now (java.util.Date. 3000)})]
      (is (= 1 (:observations-written first-cycle)))
      (is (= 0 (:observations-written second-cycle)))
      (is (= 4 (count (:records (:state second-cycle))))
          "seat, claim, file observation, and first-run commit baseline")
      (is (= "seat:codex-11:session-1"
             (-> second-cycle :projection :dirty-sets first :seat/id)))
      (is (empty? (-> second-cycle :projection :ambiguous))))))

(deftest malformed-witness-does-not-create-or-replace-state
  (let [repo (init-repo)
        state-path (.getPath (io/file repo ".git" "inbox-zero.edn"))
        witness-path (io/file repo ".git" "bad-witnesses")]
    (.mkdirs witness-path)
    (spit (io/file witness-path "bad.edn") "{:record/type")
    (is (thrown? Exception
                 (watcher/run-cycle!
                  {:state-path state-path :witness-path (.getPath witness-path)
                   :roots [{:path (.getPath repo) :label "fixture"}]})))
    (is (not (.exists (io/file state-path))))))

(deftest sender-posts-only-eligible-set-with-exact-seat
  (let [seat (seat-record)
        worktree "worktree:test"
        members (mapv (fn [n] {:path (str n ".clj")
                               :observation/id (str "o:" n)
                               :claim/id (str "c:" n)
                               :dirty-since (java.util.Date. 1000)}) (range 5))
        dirty-set {:seat/id (:seat/id seat) :repo/id "fixture"
                   :worktree/id worktree :members members :count 5
                   :oldest-dirty-at (java.util.Date. 1000)}
        posted (atom nil)]
    (with-redefs [http/post (fn [url request]
                              (reset! posted [url (json/parse-string (:body request) true)])
                              {:status 200 :body "{\"ok\":true}"})]
      (let [result (watcher/send-eligible-followups!
                    {:url "http://agency/api/alpha/followups"
                     :store (state/apply-record (state/empty-state) seat)
                     :projection {:dirty-sets [dirty-set]}
                     :now (java.util.Date. 2000)})]
        (is (= 1 (count result)))
        (is (= "codex-11" (get-in @posted [1 :agent])))
        (is (= "session-1" (get-in @posted [1 :session])))
        (is (= "inbox-zero" (get-in @posted [1 :type])))
        (is (= 200 (:status (first result))))))))

(deftest witness-through-git-threshold-produces-exact-http-enqueue
  (let [repo (init-repo)
        root {:path (.getPath repo) :label "fixture"}
        worktree (watcher/worktree-id (.getPath repo))
        state-path (.getPath (io/file repo ".git" "inbox-zero-e2e.edn"))
        witness-path (.getPath (io/file repo ".git" "inbox-zero-e2e-witnesses"))
        posted (atom nil)]
    (watcher/write-witness! witness-path (seat-record))
    (doseq [n (range 5)
            :let [path (str "dirty-" n ".clj")]]
      (spit (io/file repo path) (str n "\n"))
      (watcher/write-witness!
       witness-path
       (assoc (claim-record worktree path)
              :claim/id (str "claim:tool-call-" n)
              :witness/id (str "tool-call:" n))))
    (let [cycle (watcher/run-cycle!
                 {:state-path state-path :witness-path witness-path
                  :roots [root] :now (java.util.Date. 2000)})]
      (is (= 5 (:observations-written cycle)))
      (is (= 5 (get-in cycle [:projection :dirty-sets 0 :count])))
      (with-redefs [http/post (fn [url request]
                                (reset! posted [url (json/parse-string (:body request) true)])
                                {:status 200 :body "{\"ok\":true}"})]
        (is (= 1 (count (watcher/send-eligible-followups!
                         {:url "http://agency/api/alpha/followups"
                          :store (:state cycle) :projection (:projection cycle)
                          :now (java.util.Date. 2000)}))))
        (is (= ["codex-11" "session-1" "inbox-zero"]
               [(get-in @posted [1 :agent])
                (get-in @posted [1 :session])
                (get-in @posted [1 :type])]))))))

(deftest commit-step-is-forward-only-closes-only-touched-path-and-replays
  (let [repo (init-repo)
        root {:path (.getPath repo) :label "fixture"}
        worktree (watcher/worktree-id (.getPath repo))
        baseline-records (watcher/observe-commits (state/empty-state) root
                                                   (java.util.Date. 1000))
        baseline-state (state/replay baseline-records)]
    (is (= [:baseline] (mapv :cursor/reason baseline-records)))
    (is (empty? (state/records-of-type baseline-state
                                       :inbox-zero/commit-observation))
        "default first run records HEAD but does not backfill")
    (spit (io/file repo "tracked.txt") "committed change\n")
    (spit (io/file repo "unrelated.txt") "still dirty\n")
    (let [seat (seat-record)
          tracked-claim (claim-record worktree "tracked.txt")
          unrelated-claim (assoc (claim-record worktree "unrelated.txt")
                                 :claim/id "claim:tool-call-2"
                                 :witness/id "tool-call:2")
          witnessed (reduce state/apply-record baseline-state
                            [seat tracked-claim unrelated-claim])
          dirty-records (watcher/observe-repo witnessed root (java.util.Date. 2000))
          dirty-state (reduce state/apply-record witnessed dirty-records)]
      (git! repo "add" "tracked.txt")
      (git! repo "commit" "-q" "-m" "commit one claimed path")
      (let [commit-records (watcher/observe-commits dirty-state root
                                                     (java.util.Date. 3000))
            commit-observations (filterv #(= :inbox-zero/commit-observation
                                               (:record/type %))
                                         commit-records)
            committed (reduce state/apply-record dirty-state commit-records)
            links (projection/derive-session-commit-links
                   committed commit-observations (java.util.Date. 3000))
            linked (reduce state/apply-record committed links)
            clean-records (watcher/observe-repo linked root (java.util.Date. 3000))
            final-state (reduce state/apply-record linked clean-records)]
        (is (= 1 (count commit-observations)))
        (is (= ["tracked.txt"] (:paths (first commit-observations))))
        (is (= 1 (count links)))
        (is (= :complete (:coverage (first links))))
        (is (= :clean
               (:git/status (first (filter #(= "tracked.txt" (:path %))
                                           clean-records))))
            "the touched path receives a fresh clean observation")
        (is (= :untracked
               (:git/status (first (filter #(= "unrelated.txt" (:path %))
                                           clean-records))))
            "the new HEAD may refresh but must not close unrelated dirt")
        (is (= :untracked
               (:git/status (get (projection/current-observations final-state)
                                 [worktree "unrelated.txt"]))))
        (is (empty? (watcher/observe-commits final-state root
                                              (java.util.Date. 4000))))
        (is (empty? (watcher/observe-repo final-state root
                                           (java.util.Date. 4000))))
        (is (= 1 (count (state/records-of-type
                         final-state :inbox-zero/commit-observation))))
        (is (= 1 (count (state/records-of-type
                         final-state :inbox-zero/session-commit-link))))))))

(deftest explicit-first-run-lookback-backfills-oldest-to-newest
  (let [repo (init-repo)
        root {:path (.getPath repo) :label "fixture" :commit-lookback 2}]
    (doseq [n [1 2]]
      (spit (io/file repo "tracked.txt") (str "revision " n "\n"))
      (git! repo "add" "tracked.txt")
      (git! repo "commit" "-q" "-m" (str "revision " n)))
    (let [records (watcher/observe-commits (state/empty-state) root
                                           (java.util.Date. 1000))
          observations (filterv #(= :inbox-zero/commit-observation
                                     (:record/type %)) records)]
      (is (= [:baseline :advance]
             (mapv :cursor/reason
                   (filter #(= :inbox-zero/commit-scan-cursor (:record/type %))
                           records))))
      (is (= 2 (count observations)))
      (is (= ["revision 1" "revision 2"]
             (mapv #(str/trim (:out (git! repo "show" "-s" "--format=%s"
                                          (:commit/sha %))))
                   observations))))))

(deftest rewritten-history-rebaselines-loudly-without-observations
  (let [repo (init-repo)
        root {:path (.getPath repo) :label "fixture"}
        baseline-records (watcher/observe-commits (state/empty-state) root
                                                   (java.util.Date. 1000))
        baseline-state (state/replay baseline-records)
        root-sha (str/trim (:out (git! repo "rev-parse" "HEAD")))]
    (spit (io/file repo "tracked.txt") "old branch\n")
    (git! repo "add" "tracked.txt")
    (git! repo "commit" "-q" "-m" "old branch")
    (let [advanced-records (watcher/observe-commits baseline-state root
                                                     (java.util.Date. 2000))
          advanced-state (reduce state/apply-record baseline-state advanced-records)
          old-head (:cursor/id (get (projection/current-commit-cursors advanced-state)
                                    (watcher/worktree-id (.getPath repo))))]
      (git! repo "reset" "--hard" "-q" root-sha)
      (spit (io/file repo "tracked.txt") "new branch\n")
      (git! repo "add" "tracked.txt")
      (git! repo "commit" "-q" "-m" "new branch")
      (let [records (watcher/observe-commits advanced-state root
                                             (java.util.Date. 3000))]
        (is (= 1 (count records)))
        (is (= :rebaseline-rewrite (:cursor/reason (first records))))
        (is (= old-head (:prior/cursor-id (first records))))
        (is (empty? (filter #(= :inbox-zero/commit-observation (:record/type %))
                            records)))))))

(deftest non-repo-root-is-skipped-with-reason-and-cycle-completes
  (let [repo (init-repo)
        plain (temp-dir "inbox-zero-not-a-repo")
        state-file (io/file (temp-dir "inbox-zero-state") "state.edn")]
    (spit (io/file repo "untracked.txt") "dirt\n")
    (spit (io/file plain "orphan.txt") "content outside git\n")
    (let [result (watcher/run-cycle!
                  {:state-path (.getPath state-file)
                   :roots [{:path (.getPath plain) :label "unrepo"}
                           {:path (.getPath repo) :label "fixture"}]})]
      (is (= [{:root (.getPath plain) :label "unrepo"
               :reason :not-a-git-repository}]
             (:skipped-roots result))
          "the non-repo root is skipped loudly, without detail noise")
      (is (.exists state-file) "state is persisted despite the bad root")
      (is (some #(= "untracked.txt" (:path %))
                (vals (projection/current-observations (:state result))))
          "the healthy root is still observed"))))

(deftest git-error-in-one-root-is-isolated-and-names-itself
  (let [repo (init-repo)
        empty-repo (temp-dir "inbox-zero-empty-repo")
        state-file (io/file (temp-dir "inbox-zero-state") "state.edn")]
    (git! empty-repo "init" "-q")
    (spit (io/file repo "untracked.txt") "dirt\n")
    (let [result (watcher/run-cycle!
                  {:state-path (.getPath state-file)
                   :roots [{:path (.getPath empty-repo) :label "no-commits"}
                           {:path (.getPath repo) :label "fixture"}]})
          [skip :as skips] (:skipped-roots result)]
      (is (= 1 (count skips)))
      (is (= [:git-error (.getPath empty-repo)]
             [(:reason skip) (:root skip)]))
      (is (string? (get-in skip [:detail :message]))
          "the skip carries the failing command's story")
      (is (some #(= "untracked.txt" (:path %))
                (vals (projection/current-observations (:state result))))
          "the healthy root is still observed"))))

(deftest commit-merged-back-after-rewrite-is-observed-once
  ;; The bad case of 2026-10-14: a commit observed on one line, rebaselined
  ;; away by a history rewrite, then merged back into HEAD's line. Its record
  ;; already exists under the same [worktree sha] id; re-emitting it with a
  ;; new :observed-at made apply-record refuse the whole cycle, every cycle.
  (let [repo (init-repo)
        root {:path (.getPath repo) :label "fixture"}
        base (str/trim (:out (git! repo "rev-parse" "HEAD")))
        state0 (state/replay (watcher/observe-commits (state/empty-state) root
                                                      (java.util.Date. 1000)))]
    (spit (io/file repo "a.txt") "a\n")
    (git! repo "add" "a.txt")
    (git! repo "commit" "-q" "-m" "commit A")
    (let [a-sha (str/trim (:out (git! repo "rev-parse" "HEAD")))
          records-a (watcher/observe-commits state0 root (java.util.Date. 2000))
          state1 (reduce state/apply-record state0 records-a)]
      (is (= [a-sha] (mapv :commit/sha (filter :commit/sha records-a))))
      ;; history rewrite: HEAD moves to a line that does not contain A
      (git! repo "reset" "-q" "--hard" base)
      (spit (io/file repo "b.txt") "b\n")
      (git! repo "add" "b.txt")
      (git! repo "commit" "-q" "-m" "commit B")
      (let [records-b (watcher/observe-commits state1 root (java.util.Date. 3000))
            state2 (reduce state/apply-record state1 records-b)]
        (is (= [:rebaseline-rewrite] (mapv :cursor/reason records-b)))
        ;; A comes back into HEAD's line by a merge
        (git! repo "merge" "-q" "--no-edit" a-sha)
        (let [merge-sha (str/trim (:out (git! repo "rev-parse" "HEAD")))
              records-m (watcher/observe-commits state2 root (java.util.Date. 4000))
              state3 (reduce state/apply-record state2 records-m)]
          (is (= [merge-sha] (mapv :commit/sha (filter :commit/sha records-m)))
              "the merge commit is new; A is already observed and is not re-emitted")
          (is (= merge-sha
                 (:cursor/sha (get (projection/current-commit-cursors state3)
                                   (watcher/worktree-id (.getPath repo)))))
              "the cursor advances past the merged-back commit"))))))

(deftest record-conflict-in-one-root-is-skipped-with-reason-and-cycle-completes
  (let [repo-a (init-repo)
        repo-b (init-repo)
        state-file (io/file (temp-dir "inbox-zero-state") "state.edn")
        roots [{:path (.getPath repo-a) :label "conflicting"}
               {:path (.getPath repo-b) :label "healthy"}]
        first-run (watcher/run-cycle! {:state-path (.getPath state-file) :roots roots})
        worktree-a (watcher/worktree-id (.getPath repo-a))
        stored-cursor (->> (state/records-of-type (:state first-run)
                                                  :inbox-zero/commit-scan-cursor)
                           (filter #(= worktree-a (:worktree/id %)))
                           first)
        ;; same id as a stored record, different content: what the store refuses
        conflicting (assoc stored-cursor :observed-at (java.util.Date. 999))
        original watcher/observe-commits]
    (is (map? stored-cursor))
    (spit (io/file repo-b "untracked.txt") "dirt\n")
    (with-redefs [watcher/observe-commits
                  (fn [store root now]
                    (if (= (.getPath repo-a) (:path root))
                      [conflicting]
                      (original store root now)))]
      (let [result (watcher/run-cycle! {:state-path (.getPath state-file) :roots roots})
            [skip :as skips] (:skipped-roots result)]
        (is (= 1 (count skips)))
        (is (= [:record-conflict (.getPath repo-a)] [(:reason skip) (:root skip)]))
        (is (= :inbox-zero/id-conflict (get-in skip [:detail :error/type])))
        (is (= (:cursor/id stored-cursor) (get-in skip [:detail :record/id]))
            "the skip names the refused record")
        (is (some #(= "untracked.txt" (:path %))
                  (vals (projection/current-observations (:state result))))
            "the healthy root is still observed and persisted")))))
