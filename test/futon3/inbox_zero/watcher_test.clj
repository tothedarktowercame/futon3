(ns futon3.inbox-zero.watcher-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.test :refer [deftest is]]
            [babashka.http-client :as http]
            [cheshire.core :as json]
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
      (is (= 3 (count (:records (:state second-cycle)))))
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
