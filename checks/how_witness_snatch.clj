(ns how-witness-snatch
  "A DERIVED behavioural check for one `@how` edge in `library/snatch/`.

   Follows the third verification layer of `futon2.aif.operational-witness`:
   a witness is a relation run FORWARD against stated facts, ungameable in the
   sense that a goal either succeeds or it does not.

   The edge under test:

     snatch/protect-the-unprotected-move  @how  snatch/preserve-the-right-to-abstain

   `protect-the-unprotected-move` says: compare arrangements by how much
   first-mover exposure each leaves standing. `preserve-the-right-to-abstain`
   claims to be a method for that. The edge is ATTESTED iff removing abstention
   strictly worsens P1's guaranteed outcome — i.e. iff abstention is what was
   protecting the first mover.

   Facts are read off the game's own design diagrams (g1-no-property.mmd,
   g2-counterproductive-rule.mmd): own tokens score 1, the other's score 2,
   P1 holds 10, and a snatch takes the offer and pays nothing."
  (:require [clojure.core.logic :as l]))

;; ---- stated facts -------------------------------------------------------
;; [treatment action-available-to-P1]
(def p1-actions
  [[:g1 :abstain] [:g1 :offer]
   [:g2 :offer]                       ; "No ofrecer" disabled when forcedByP2
   [:g3 :abstain] [:g3 :offer]
   [:g4 :abstain] [:g4 :offer]
   [:g5 :abstain] [:g5 :offer]])

;; [treatment guaranteed-floor] — P1's worst case over P2's replies, best policy.
;; Abstaining guarantees the 10 own tokens at 1 point each.
;; Forced to offer at least one token, a snatch leaves 10 - x.
(def guaranteed-floor
  [[:g1 10] [:g2 9] [:g3 10] [:g4 10] [:g5 10]])

;; ---- the relation -------------------------------------------------------
(defn abstention-availableo [t]
  (l/membero [t :abstain] p1-actions))

(defn flooro [t f] (l/membero [t f] guaranteed-floor))

(defn exposure-reduced-by-abstentiono
  "Relation: treatment `with` preserves abstention, `without` removes it, and
   the floor is strictly higher where abstention survives. Binds the explaining
   pair of floors."
  [with without fw fo]
  (l/all
   (abstention-availableo with)
   (l/nafc abstention-availableo without)
   (flooro with fw)
   (flooro without fo)
   (l/project [fw fo] (l/== true (> fw fo)))))

(defn witness-how-edge []
  (l/run* [with without fw fo]
    (exposure-reduced-by-abstentiono with without fw fo)))

;; ---- the mirror ---------------------------------------------------------
;; Same relation asked of two treatments that BOTH preserve abstention.
;; A real witness must find nothing here.
(defn mirror []
  (l/run* [fw fo]
    (l/fresh [with without]
      (l/== with :g1) (l/== without :g3)
      (exposure-reduced-by-abstentiono with without fw fo))))

(defn -main [& _]
  (let [w (witness-how-edge) m (mirror)]
    (println "witness bindings [with without floor-with floor-without]:")
    (doseq [b w] (println "  " b))
    (println "mirror bindings (must be empty):" m)
    (println)
    (println (if (and (seq w) (empty? m))
               "ATTESTED — the @how edge holds behaviourally, and the mirror is silent."
               "NOT ATTESTED"))))
