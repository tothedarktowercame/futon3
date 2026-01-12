(ns futon3.musn.schema
  "MUSN endpoint and event schemas (Malli). These are used for validation of live, server-managed runs."
  (:require [malli.core :as m]
            [malli.util :as mu]))

(def PatternId [:re #"^[a-z0-9._-]+/[a-z0-9._-]+$"])
(def FilePath string?)
(def Anchor [:map
             [:anchor/type keyword?]
             [:anchor/ref map?]])

(def Reason
  [:map
   [:mode keyword?]                                 ;; :use, :explore, :read-only
   [:reads {:optional true} [:vector PatternId]]
   [:note {:optional true} string?]
   [:source {:optional true} keyword?]              ;; :hud, :auto, :explicit
   [:aif {:optional true} [:map
                           [:G {:optional true} number?]
                           [:tau {:optional true} number?]
                           [:e {:optional true} number?]
                           [:o {:optional true} number?]]]])

(def SessionCreateReq
  [:map
   [:client [:map [:id string?]
             [:caps {:optional true} [:vector keyword?]]]]
   [:session/id {:optional true} string?]])

(def SessionCreateResp
  [:map
   [:ok boolean?]
   [:session/id string?]
   [:policy [:map
             [:off-trail [:map [:free number?] [:ratio number?] [:action string?]]]
             [:trail-allow {:optional true} [:map
                                             [:patterns {:optional true} [:vector PatternId]]
                                             [:namespaces {:optional true} [:vector string?]]]]
             [:aif-config {:optional true} map?]]]])

(def TurnStartReq
  [:map
   [:session/id string?]
   [:turn int?]
   [:hud {:optional true} [:map
                           [:candidates {:optional true} [:vector PatternId]]
                           [:scores {:optional true} [:map-of PatternId number?]]]]])

(def TurnStartResp
  [:map
   [:ok boolean?]
   [:turn int?]
   [:allowed [:map [:patterns [:vector PatternId]]
              [:namespaces {:optional true} [:vector string?]]]]
   [:aif {:optional true} [:map
                           [:tau number?]
                           [:G-scores {:optional true} [:map-of PatternId number?]]]]
   [:trail [:map [:on int?] [:off int?] [:limit number?]]]])

(def TurnPlanReq [:map [:session/id string?] [:turn int?] [:plan string?]])
(def TurnPlanResp [:map [:ok boolean?] [:plan/accepted boolean?]])

(def TurnSelectReq
  [:map
   [:session/id string?]
   [:turn int?]
   [:candidates [:vector PatternId]]
   [:chosen PatternId]
   [:reason Reason]
   [:anchors {:optional true} [:vector Anchor]]])

(def TurnSelectResp
  [:map
   [:ok boolean?]
   [:psr [:map
          [:decision/id string?]
          [:pattern/id PatternId]
          [:candidates [:vector PatternId]]
          [:selection/reason Reason]
          [:selection/anchors {:optional true} [:vector Anchor]]]]])

(def TurnActionReq
  [:map
   [:session/id string?]
   [:turn int?]
   [:pattern/id PatternId]
   [:action [:enum "read" "implement" "update" "off-trail" "wide-scan"]]
   [:note {:optional true} string?]
   [:files {:optional true} [:vector FilePath]]
   [:source {:optional true} keyword?]             ;; :auto, :explicit, :hud, :system
   [:cost {:optional true} [:enum :cheap :expensive :human-contact]]])

(def TurnActionResp
  [:map
   [:ok boolean?]
   [:trail {:optional true} [:map [:on int?] [:off int?] [:limit number?]]]
   [:aif {:optional true} [:map
                           [:e {:optional true} number?]
                           [:dE {:optional true} number?]]]
   [:warning {:optional true} [:map [:type keyword?] [:msg string?]]]])

(def TurnUseReq
  [:map
   [:session/id string?]
   [:turn int?]
   [:pattern/id PatternId]
   [:anchors {:optional true} [:vector Anchor]]
   [:note {:optional true} string?]
   [:inferred? {:optional true} boolean?]])

(def TurnUseResp
  [:map
   [:ok boolean?]
   [:pur [:map
          [:pattern/id PatternId]
          [:use/reason {:optional true} string?]
          [:outcome/tags {:optional true} [:vector keyword?]]]]])

(def EvidenceAddReq
  [:map
   [:session/id string?]
   [:turn int?]
   [:pattern/id PatternId]
   [:files [:vector FilePath]]
   [:note string?]])

(def EvidenceAddResp [:map [:ok boolean?] [:record/id string?]])

(def TurnEndReq [:map [:session/id string?] [:turn int?]])

(def TurnEndResp
  [:map
   [:ok boolean?]
   [:summary [:map
              [:actions map?]
              [:trail [:map [:on int?] [:off int?]]]
              [:psr {:optional true} PatternId]
              [:pur {:optional true} PatternId]
              [:warnings int?]]]
   [:halt? boolean?]
   [:halt/reason {:optional true} map?]])

;; Convenient registry for validation lookup.
(def registry
  {:pattern/id PatternId
   :turn/anchor Anchor
   :turn/reason Reason
   :session/create-req SessionCreateReq
   :session/create-resp SessionCreateResp
   :turn/start-req TurnStartReq
   :turn/start-resp TurnStartResp
   :turn/plan-req TurnPlanReq
   :turn/plan-resp TurnPlanResp
   :turn/select-req TurnSelectReq
   :turn/select-resp TurnSelectResp
   :turn/action-req TurnActionReq
   :turn/action-resp TurnActionResp
   :turn/use-req TurnUseReq
   :turn/use-resp TurnUseResp
   :evidence/add-req EvidenceAddReq
   :evidence/add-resp EvidenceAddResp
   :turn/end-req TurnEndReq
   :turn/end-resp TurnEndResp})

(defn validator
  "Build a malli validator for the named schema keyword in registry."
  [k]
  (m/validator (registry k)))
