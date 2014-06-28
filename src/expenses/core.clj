(ns expenses.core
    (:require [clojure.algo.generic.functor :refer [fmap]]))

;; ---------------------------
;; Krakow Euroclojure Expenses
;; ---------------------------

(expenses
 "Accomodation" M -> GBP278 -> [M P D L G]
 "Taxi" L -> PLN100 -> [M D L G]
 "Beer & Pizza" M -> PLN150 -> [M G D L]
 "Coffee" G -> PLN26 -> [G D L]
 "Beer for home" L -> PLN32 -> [M D L G]
 "Supermaket" M -> PLN40 -> [M D L G P]
 "Dinner" D -> PLN200 -> [M D L]
 "Dinner" D -> PLN200 -> [M D L G]
 "Dinner" M -> PLN20 -> [M D L G]
 "Beer" G -> PLN70 -> [M D L G P]
 "Beer" D -> PLN50 -> [M D L G P]
 "Beer" P -> PLN10 -> [M D L G P]
 "Breakfast" P -> PLN69 -> [M D P]
 "Shakes" P -> PLN55 -> [M D P L]
 "Shakes" L -> PLN25 -> [M D P L]
 "Shopping" P -> PLN20 -> [M D P L]
 "Pizza" M -> PLN89 -> [M P D L]
 "Shopping" L -> PLN29 -> [M P D L])

;; ---------------------------
;; M: Martin
;; L: Lloyd
;; G: Greg
;; D: Dan
;; P: Paddy
;; ---------------------------

(def payments
 [{:payer "M"
   :beneficiaries ["M" "D" "P"]
   :currency "GBP"
   :amount 150}
  {:payer "D"
   :beneficiaries ["M" "D" "P"]
   :currency "GBP"
   :amount 150}
  {:payer "P"
   :beneficiaries ["M" "P"]
   :currency "GBP"
   :amount 50}])

(defn payment-splitter [payment]
  (fn [beneficiary]
    (dissoc
     (assoc payment
       :amount (/ (:amount payment) (count (:beneficiaries payment)))
       :beneficiary beneficiary)
     :beneficiaries)))

(defn splat-payments [payments]
  (mapcat (fn [payment]
            (map (payment-splitter payment)
                 (:beneficiaries payment)))
          payments))

(defn accumulate-amounts [role payments]
    (fmap #(apply + (map :amount %)) (group-by role payments)))

(def accumulate-amounts-paid
  (partial accumulate-amounts :payer))

(def accumulate-amounts-received
  (partial accumulate-amounts :beneficiary))

(defn calculate-balances  [payments]
  (let [splat-payments (splat-payments payments)
        amounts-paid (accumulate-amounts-paid splat-payments)
        amounts-received (accumulate-amounts-received splat-payments)]
    (merge-with + amounts-paid (fmap - amounts-received))))

;; Split into 2 lists

(defn owed [balances] (filter (comp pos? second) balances))
(defn owing [balances] (filter (comp neg? second) balances))

(defn reconcilliation [ower owed amount]
  {:ower ower :owed owed :amount amount})

(defn reconcile [payments]
  (println "payments> " payments)
  (let [balances (calculate-balances payments)]
    (println "balances> " balances)
    (loop [remaining-owed (seq (owed balances))
           owing (map #(update-in % [1] -) (owing balances)) ;; e.g. ([D 1834/15] [L 1834/15] [P 278/5])
           result []]
      (println "reconcile> " remaining-owed " // " owing " // " result)
      (if-let [next-owed (first remaining-owed)] ;; e.g. [M 1112/5]
        (do
          (println " - next-owed: " next-owed)
          (let [total-owing-so-far-seq (reductions + (map second owing))
                owing-with-total-owed-so-far (map vector total-owing-so-far-seq owing)
                ;; e.g. owing-with-total-owed-so-far:
                ;; ([1834/15 [D 1834/15]] [3668/15 [L 1834/15]] [4502/15 [P 278/5]])
                not-enough-to-pay-off-owed #(< (first %) (second next-owed))
                fully-paid-up (take-while not-enough-to-pay-off-owed owing-with-total-owed-so-far)
                not-fully-paid-up (drop-while not-enough-to-pay-off-owed owing-with-total-owed-so-far)
                paying-remainder (first not-fully-paid-up)
                not-paying-this-time (next not-fully-paid-up)
                next-result (let [new-reconcilliations (map reconcilliation
                                                            (map (comp first second) fully-paid-up)
                                                            (repeatedly #(first next-owed))
                                                            (map (comp second second) fully-paid-up))]
                              (if (not (empty? new-reconcilliations))
                                (apply conj result new-reconcilliations)
                                result))
                remainder (- (first paying-remainder) (second next-owed))
                next-result (conj next-result (reconcilliation (first (second paying-remainder))
                                                               (first next-owed)
                                                               (- (second (second paying-remainder)) remainder)))]
            (println " - paying-remainder: " paying-remainder " // remainder: " remainder)
            (recur (next remaining-owed)
                   (if (> remainder 0)
                     (conj (map second not-paying-this-time) (update-in (second paying-remainder) [1] #(- % remainder)))
                     (map second not-paying-this-time))
                   next-result)))
        result))))

;;=> [ {:owed G :ower P :amount 23} {:owed G :ower M :amount 23} ]

;; (defn -main []
;;   (splat-payments payments))

  ;; [{G [:paid 234 :received] 67567
  ;;   D []}]

  ;;[ "Martin owes Greg GBP27.83"
  ;;  "Dan owes Lloyd GBP36.87"]
