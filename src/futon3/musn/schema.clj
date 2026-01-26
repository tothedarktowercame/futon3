(ns futon3.musn.schema
  "MUSN endpoint and event schemas (Malli). These are used for validation of live, server-managed runs."
  (:require [malli.core :as m]))

(def PatternId [:re #"^[a-z0-9._-]+/[a-z0-9._-]+$"])
(def TurnId [:int])
(def PatternIdKey [:or PatternId keyword?])
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
   [:deviation {:optional true}                     ;; justification when overriding AIF suggestion
    [:map
     [:aif-suggested PatternId]
     [:reason string?]]]
   [:aif {:optional true}
    [:map
     [:G {:optional true} number?]
     [:tau {:optional true} number?]
     [:e {:optional true} number?]
     [:o {:optional true} number?]
     [:G-rejected {:optional true} [:map-of PatternId number?]]]]])

(def AifSelectionSummary
  [:map
   [:G {:optional true} number?]
   [:tau {:optional true} number?]
   [:G-rejected {:optional true} [:map-of PatternId number?]]])

(def ManaState
  [:map
   [:budget number?]
   [:balance number?]
   [:earned number?]
   [:spent number?]
   [:turn {:optional true} int?]
   [:last-change {:optional true} map?]])

(def SessionCreateReq
  [:map
   [:client [:map [:id string?]
             [:caps {:optional true} [:vector keyword?]]]]
   [:session/id {:optional true} string?]
   [:policy {:optional true} map?]
   [:lab/root {:optional true} string?]])

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
   [:turn TurnId]
   [:hud {:optional true} [:map {:closed false}
                           [:candidates {:optional true} [:vector PatternId]]
                           [:scores {:optional true} [:map-of PatternIdKey number?]]
                           [:candidate-details {:optional true} [:vector map?]]
                           [:intent {:optional true} string?]
                           [:sigils {:optional true} [:vector map?]]
                           [:namespaces {:optional true} [:vector string?]]
                           [:aif {:optional true} map?]]]])

(def TurnStartResp
  [:map
   [:ok boolean?]
   [:turn TurnId]
   [:allowed [:map [:patterns [:vector PatternId]]
              [:namespaces {:optional true} [:vector string?]]]]
   [:aif {:optional true} [:map
                           [:tau number?]
                           [:G-scores {:optional true} [:map-of PatternId number?]]]]
   [:mana {:optional true} ManaState]
   [:trail [:map [:on int?] [:off int?] [:limit number?]]]])

(def TurnPlanReq
  [:map
   [:session/id string?]
   [:turn TurnId]
   [:plan {:optional true} string?]
   [:plan/type {:optional true} keyword?]
   [:plan/diagram {:optional true} map?]
   [:plan/components-path {:optional true} string?]
   [:plan/context {:optional true} map?]
   [:plan/eval-config {:optional true} map?]])
(def TurnPlanResp
  [:map
   [:ok boolean?]
   [:plan/accepted boolean?]
   [:plan/warnings {:optional true} [:vector string?]]
   [:plan/eval {:optional true} map?]
   [:mana {:optional true} ManaState]])

(def TurnSelectReq
  [:map
   [:session/id string?]
   [:turn TurnId]
   [:candidates [:vector PatternId]]
   [:chosen {:optional true} PatternId]
   [:reason Reason]
   [:anchors {:optional true} [:vector Anchor]]])

(def TurnSelectResp
  [:map
   [:ok boolean?]
   [:psr {:optional true} [:map
                           [:decision/id string?]
                           [:pattern/id PatternId]
                           [:candidates [:vector PatternId]]
                           [:selection/reason Reason]
                           [:selection/anchors {:optional true} [:vector Anchor]]]]
   [:aif {:optional true} AifSelectionSummary]])

(def TurnActionReq
  [:map
   [:session/id string?]
   [:turn TurnId]
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
   [:mana {:optional true} ManaState]
   [:warning {:optional true} [:map [:type keyword?] [:msg string?]]]
   [:logic {:optional true} map?]])

(def TurnUseReq
  [:map
   [:session/id string?]
   [:turn TurnId]
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
          [:outcome/tags {:optional true} [:vector keyword?]]]]
   [:aif {:optional true} map?]
   [:mana {:optional true} ManaState]])

(def EvidenceAddReq
  [:map
   [:session/id string?]
   [:turn TurnId]
   [:pattern/id PatternId]
   [:files [:vector FilePath]]
   [:note string?]])

(def EvidenceAddResp [:map [:ok boolean?] [:record/id string?]])

(def TurnEndReq [:map [:session/id string?] [:turn TurnId]])

(def TurnEndResp
  [:map
   [:ok boolean?]
   [:summary [:map
              [:actions map?]
              [:trail [:map [:on int?] [:off int?]]]
              [:psr {:optional true} PatternId]
              [:pur {:optional true} PatternId]
              [:warnings int?]
              [:mana/donations {:optional true}
               [:map
                [:count int?]
                [:total number?]
                [:failed int?]
                [:pending {:optional true} number?]]]]]
   [:halt? boolean?]
   [:halt/reason {:optional true} map?]
   [:logic {:optional true} map?]
   [:mana {:optional true} ManaState]])

(def TurnResumeReq [:map [:session/id string?] [:turn TurnId] [:note {:optional true} string?]])
(def TurnResumeResp [:map [:ok boolean?] [:turn TurnId]])

(def RoomId string?)

(def ChatAuthor
  [:map
   [:id string?]
   [:name {:optional true} string?]])

(def ChatMessageReq
  [:map
   [:room RoomId]
   [:msg-id {:optional true} string?]
   [:author ChatAuthor]
   [:text string?]
   [:lab/root {:optional true} string?]])

(def ChatMessageResp
  [:map
   [:ok boolean?]
   [:room RoomId]
   [:chat map?]
   [:paused? boolean?]
   [:pause {:optional true} map?]
   [:seq int?]])

(def ChatBidReq
  [:map
   [:room RoomId]
   [:msg-id {:optional true} string?]
   [:bidder ChatAuthor]
   [:note {:optional true} string?]
   [:lab/root {:optional true} string?]])

(def ChatBidResp
  [:map
   [:ok boolean?]
   [:room RoomId]
   [:bid map?]
   [:paused? boolean?]
   [:pause {:optional true} map?]
   [:seq int?]])

(def ChatUnlatchReq
  [:map
   [:room RoomId]
   [:msg-id {:optional true} string?]
   [:bid/id {:optional true} string?]
   [:by {:optional true} ChatAuthor]
   [:lab/root {:optional true} string?]])

(def ChatUnlatchResp
  [:map
   [:ok boolean?]
   [:room RoomId]
   [:accepted {:optional true} map?]
   [:paused? boolean?]
   [:pause {:optional true} map?]
   [:seq int?]])

(def ChatStateReq
  [:map
   [:room RoomId]
   [:since {:optional true} int?]
   [:lab/root {:optional true} string?]])

(def ChatStateResp
  [:map
   [:ok boolean?]
   [:room RoomId]
   [:state map?]
   [:events [:vector map?]]
   [:cursor int?]])

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
   :turn/end-resp TurnEndResp
   :turn/resume-req TurnResumeReq
   :turn/resume-resp TurnResumeResp
   :chat/message-req ChatMessageReq
   :chat/message-resp ChatMessageResp
   :chat/bid-req ChatBidReq
   :chat/bid-resp ChatBidResp
   :chat/unlatch-req ChatUnlatchReq
   :chat/unlatch-resp ChatUnlatchResp
   :chat/state-req ChatStateReq
   :chat/state-resp ChatStateResp})

(defn validator
  "Build a malli validator for the named schema keyword in registry."
  [k]
  (m/validator (registry k)))
