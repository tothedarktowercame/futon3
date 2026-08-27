(ns how-kernel-snatch
  "PILOT: the same @how edge as `how-witness-snatch`, read as a KERNEL rather
   than a relation — step 2-5 of the E-R5 build, in the Snatch domain.

   `Rel` is copy-discard but not Markov: a relation may be partial and lose
   mass. A kernel is total. So the move from `run*` to a distribution is the
   move from WHICH outcomes are possible to HOW MUCH mass each carries.

     support  ← the core.logic relation (unchanged, reused)
     mass     ← a Beta posterior per edge, Beta(1,1) when unattested
     kernel   ← support × mass, normalised
     readouts ← entropy (the ambiguity term) and discriminates?
     mirror   ← must carry ZERO mass

   Beta(1,1) is a PRIOR and therefore a stipulation: declared here, per S-G3,
   not absorbed into a number."
  (:require [clojure.core.logic :as l]))

;; ---- domain facts (Snatch; from the game's design diagrams) --------------
(def p1-actions
  [[:g1 :abstain] [:g1 :offer] [:g2 :offer]
   [:g3 :abstain] [:g3 :offer] [:g4 :abstain] [:g4 :offer]
   [:g5 :abstain] [:g5 :offer]])
(def guaranteed-floor [[:g1 10] [:g2 9] [:g3 10] [:g4 10] [:g5 10]])

;; ---- step 1: support, unchanged from the relational witness --------------
(defn abstention-availableo [t] (l/membero [t :abstain] p1-actions))
(defn flooro [t f] (l/membero [t f] guaranteed-floor))

(defn supporto [with without fw fo]
  (l/all (abstention-availableo with)
         (l/nafc abstention-availableo without)
         (flooro with fw) (flooro without fo)
         (l/project [fw fo] (l/== true (> fw fo)))))

(defn support [] (l/run* [w o fw fo] (supporto w o fw fo)))
(defn mirror-support []
  (l/run* [fw fo] (l/fresh [w o] (l/== w :g1) (l/== o :g3) (supporto w o fw fo))))

;; ---- step 2: mass -------------------------------------------------------
(def ^:const prior-alpha 1.0)   ; DECLARED stipulation (S-G3)
(def ^:const prior-beta  1.0)

(defn beta-mean [a b] (/ a (+ a b)))

(defn kernel
  "support × mass, normalised. `evidence` maps a support element to [successes
   failures]; absent ⇒ Beta(1,1) ⇒ uniform, i.e. no reason to prefer any way
   the edge holds."
  ([sup] (kernel sup {}))
  ([sup evidence]
   (let [w (mapv (fn [b] (let [[s f] (get evidence b [0 0])]
                           (beta-mean (+ prior-alpha s) (+ prior-beta f)))) sup)
         z (reduce + w)]
     (if (zero? z) {} (zipmap sup (map #(/ % z) w))))))

;; ---- step 4: readouts ---------------------------------------------------
(defn entropy [k]
  (- (reduce + (map (fn [p] (if (pos? p) (* p (Math/log p)) 0.0)) (vals k)))))

(defn discriminates?
  "The channel is non-constant: two inputs give different distributions."
  [k1 k2] (not= k1 k2))

(defn- fmt [x] (format "%.4f" (double x)))

(defn -main [& _]
  (let [sup (support)
        k0  (kernel sup)                                   ; unattested
        ev  {(first sup) [3 0] (second sup) [0 2]}         ; attestation arrives
        k1  (kernel sup ev)
        mk  (kernel (mirror-support))]
    (println "support (" (count sup) "bindings ):")
    (doseq [b sup] (println "   " b))
    (println)
    (println "UNATTESTED  Beta(1,1) — uniform")
    (doseq [[b p] k0] (println "   " b "->" (fmt p)))
    (println "   entropy =" (fmt (entropy k0)) "nats   (log 4 =" (fmt (Math/log 4)) ")")
    (println)
    (println "ATTESTED    first binding 3 successes, second 2 failures")
    (doseq [[b p] k1] (println "   " b "->" (fmt p)))
    (println "   entropy =" (fmt (entropy k1)) "nats")
    (println)
    (println "MIRROR mass:" (if (empty? mk) "ZERO — the kernel cannot fire" mk))
    (println "discriminates? (unattested vs attested):" (discriminates? k0 k1))
    (println)
    (println (if (and (seq k0) (empty? mk) (< (entropy k1) (entropy k0)))
               "PILOT OK — spread present, responds to attestation, mirror silent."
               "PILOT FAILED"))))
