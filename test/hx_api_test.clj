(ns hx-api-test
  (:require [clojure.test :refer :all]
            [futon3.hx.api :as api]
            [futon3.hx.store :as store]))

(deftest api-crud-roundtrip
  (let [tmp (doto (java.io.File/createTempFile "hx-api" ".edn")
              (.delete)
              (.deleteOnExit))
        original (store/current-log-path)
        artifact {:artifact/id "futon3/docs/fulab-plumbing.org"
                  :artifact/type :org
                  :artifact/title "Fulab plumbing"
                  :artifact/path "docs/fulab-plumbing.org"}
        anchor {:anchor/id "summary"
                :anchor/artifact "futon3/docs/fulab-plumbing.org"
                :anchor/kind :heading
                :anchor/selector {:kind :regex :pattern "^\\* Summary"}}
        link {:link/from {:artifact/id "futon3/docs/fulab-plumbing.org" :anchor/id "summary"}
              :link/to {:artifact/id "futon3/docs/plans/hypertext-porcelain.md" :anchor/id "goal"}
              :link/type :documents
              :link/agent :codex
              :link/rationale "plumbing doc references the plan"}
        validation {:validation/status :accepted
                    :validation/structural {:ok? true
                                            :errors []}
                    :validation/semantic {:ok? true
                                          :model "stub"
                                          :confidence 0.72
                                          :rationale "doc summarizes plan"}
                    :validation/agent :codex
                    :validation/timestamp "2026-01-06T00:02:00Z"}]
    (try
      (store/set-log-path! (.getAbsolutePath tmp))
      (store/reset-state!)
      (let [reg (api/register-artifact! artifact)]
        (is (= true (:ok reg)))
        (is (some? (:artifact/registered (:artifact reg)))))
      (let [resp (api/upsert-anchors! "futon3/docs/fulab-plumbing.org" [anchor])]
        (is (= true (:ok resp)))
        (is (= [anchor] (api/get-anchors "futon3/docs/fulab-plumbing.org"))))
      (let [resp (api/suggest-link! link)
            link-id (:link/id (:link resp))]
        (is (= true (:ok resp)))
        (is (= :suggested (:link/status (:link resp))))
        (is (= 1 (count (api/candidates))))
        (is (= true (:ok (api/accept-link! link-id :joe validation))))
        (is (= :accepted (:link/status (api/get-link link-id))))
        (is (= validation (:link/validation (api/get-link link-id)))))
      (let [bad (api/suggest-link! (assoc link :link/type :unknown-type))]
        (is (= false (:ok bad)))
        (is (= "unknown-link-type" (:err bad))))
      (finally
        (store/set-log-path! original)))))
