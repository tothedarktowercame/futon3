(ns futon3.inbox-zero.infer-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.infer :as infer]))

(def tracer
  {:repo/id "futon3c-d" :worktree/id "worktree:83d88d109136f3c8"
   :path "scripts/session-cost.py"})

(def seat "seat:claude-3:7cdc25b0-2189-4f90-801e-c517e7f37d4d")

(def tracer-evidence
  {:substrate-mentions
   [{:seat/id seat :repo/id "futon3c" :worktree/id (:worktree/id tracer)
     :path (:path tracer) :at #inst "2026-08-24T08:21:54.580Z"
     :source/id "emacs-6427" :candidate-authored? true
     :attested? true}]
   :mtimes
   [{:repo/id "wrong-on-purpose" :worktree/id (:worktree/id tracer)
     :path (:path tracer) :at #inst "2026-08-24T08:21:27.874Z"
     :source/id "stat:session-cost"}]
   :same-worktree-claims
   [{:seat/id seat :repo/id "futon3" :worktree/id (:worktree/id tracer)
     :at #inst "2026-08-24T11:12:34.598Z" :source/id "claim:781b"}]
   :activity-windows
   [{:seat/id seat :worktree/id (:worktree/id tracer)
     :from #inst "2026-08-24T08:19:09.618Z"
     :to #inst "2026-08-24T08:59:06.817Z" :source/id "window:substrate"}]})

(deftest tracer-is-corroborated-despite-repo-aliases
  (let [result (infer/infer-attribution tracer tracer-evidence)
        candidate (first (:candidates result))
        ids (set (map :source/id (:evidence candidate)))]
    (is (= :propose (:verdict result)))
    (is (= seat (:seat/id candidate)))
    (is (= 1 (:rank candidate)))
    (is (= :corroborated (:confidence candidate)))
    (is (every? ids ["emacs-6427" "stat:session-cost" "claim:781b"]))))

(deftest unattested-mention-never-corroborates
  ;; The same tracer evidence, but the mention is turn text / a tool result
  ;; rather than a write the seat issued: it drops to :weak, which is visible
  ;; but insufficient unless the caller opts in.
  (let [evidence (assoc-in tracer-evidence [:substrate-mentions 0 :attested?] false)
        result (infer/infer-attribution tracer evidence)
        candidate (first (:candidates result))]
    (is (= :insufficient (:verdict result)))
    (is (= :weak (:confidence candidate)))
    (is (not-any? #(= "emacs-6427" (:source/id %)) (:evidence candidate)))
    (is (= :propose (:verdict (infer/infer-attribution tracer evidence {:allow-weak? true}))))))

(deftest structured-write-is-direct
  (let [result (infer/infer-attribution
                tracer {:structured-writes
                        [{:seat/id seat :repo/id "another-alias"
                          :worktree/id (:worktree/id tracer) :path (:path tracer)
                          :at #inst "2026-08-24T08:21:27Z"
                          :source/id "tool-use:edit"}]})]
    (is (= :propose (:verdict result)))
    (is (= :direct (get-in result [:candidates 0 :confidence])))))

(deftest corroboration-requires-flags-and-one-shared-window
  (let [mention (first (:substrate-mentions tracer-evidence))
        mtime (first (:mtimes tracer-evidence))
        split-windows [{:seat/id seat :worktree/id (:worktree/id tracer)
                        :from #inst "2026-08-24T08:21:50Z"
                        :to #inst "2026-08-24T08:22:00Z" :source/id "mention-window"}
                       {:seat/id seat :worktree/id (:worktree/id tracer)
                        :from #inst "2026-08-24T08:21:20Z"
                        :to #inst "2026-08-24T08:21:30Z" :source/id "mtime-window"}]
        base {:same-worktree-claims (:same-worktree-claims tracer-evidence)
              :mtimes [mtime] :activity-windows split-windows}]
    (is (= :insufficient
           (:verdict (infer/infer-attribution
                      tracer (assoc base :substrate-mentions [mention])))))
    (is (= :insufficient
           (:verdict (infer/infer-attribution
                      tracer (assoc base :activity-windows (:activity-windows tracer-evidence)
                                         :substrate-mentions
                                         [(dissoc mention :candidate-authored?)])))))))

(deftest weak-requires-explicit-policy
  (let [bundle {:same-worktree-claims
                [{:seat/id seat :worktree/id (:worktree/id tracer)
                  :at #inst "2026-08-24T11:12:34Z" :source/id "claim:one"}]
                :mtimes
                [{:worktree/id (:worktree/id tracer) :path (:path tracer)
                  :at #inst "2026-08-24T08:21:27Z" :source/id "stat:path"}]
                :activity-windows
                [{:seat/id seat :worktree/id (:worktree/id tracer)
                  :from #inst "2026-08-24T08:00:00Z"
                  :to #inst "2026-08-24T09:00:00Z" :source/id "window:one"}]}
        default-result (infer/infer-attribution tracer bundle)
        allowed-result (infer/infer-attribution tracer bundle {:allow-weak? true})]
    (is (= :insufficient (:verdict default-result)))
    (is (= :weak (get-in default-result [:candidates 0 :confidence])))
    (is (= :propose (:verdict allowed-result)))))

(deftest conflicting-direct-writes-are-ambiguous
  (let [write (fn [candidate id]
                {:seat/id candidate :worktree/id (:worktree/id tracer)
                 :path (:path tracer) :at #inst "2026-08-24T08:21:27Z"
                 :source/id id})
        result (infer/infer-attribution
                tracer {:structured-writes [(write "seat:a:s1" "write:a")
                                             (write "seat:b:s2" "write:b")]})]
    (is (= :ambiguous (:verdict result)))
    (is (= ["seat:a:s1" "seat:b:s2"] (mapv :seat/id (:candidates result))))))

(deftest git-author-and-bare-mtime-are-insufficient
  (let [result (infer/infer-attribution
                tracer {:git-authorship [{:author "Joseph Corneli"
                                           :worktree/id (:worktree/id tracer)
                                           :path (:path tracer)
                                           :source/id "git:blame"}]
                        :mtimes [{:worktree/id (:worktree/id tracer)
                                  :path (:path tracer)
                                  :at #inst "2026-08-24T08:21:27Z"
                                  :source/id "stat:path"}]})]
    (is (= :insufficient (:verdict result)))
    (is (empty? (:candidates result)))))

(deftest inference-is-deterministic
  (is (= (infer/infer-attribution tracer tracer-evidence)
         (infer/infer-attribution tracer tracer-evidence))))

(deftest followup-names-key-evidence-and-confirmation-effect
  (let [result (infer/infer-attribution tracer tracer-evidence)
        text (infer/confirmation-followup-text result (first (:candidates result)))]
    (is (str/includes? text "futon3c-d:scripts/session-cost.py"))
    (is (str/includes? text "worktree:83d88d109136f3c8"))
    (is (str/includes? text "emacs-6427"))
    (is (str/includes? text "stat:session-cost"))
    (is (str/includes? text "claim:781b"))
    (is (str/ends-with?
         text
         "Confirm / reject / not sure. Confirming mints the file claim; inference alone changes nothing."))))
