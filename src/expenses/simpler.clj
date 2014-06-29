(ns expenses.simpler
  (:require [clojure.algo.generic.functor :refer [fmap]]))

(defn convert
  "Convert everything to GBP."
  [currency amount]
  (condp = currency
    "PLN" (/ amount 5)
    "GBP" amount))

;; Payments are map with keys - :payer, :beneficiaries, :currency, :amount
(def input
 [{:payer "M"
   :beneficiaries ["M" "D" "P"]
   :currency "GBP"
   :amount 300}
  {:payer "D"
   :beneficiaries ["M" "D" "P"]
   :currency "PLN"
   :amount 858}
  {:payer "P"
   :beneficiaries ["M" "P"]
   :currency "GBP"
   :amount 50}])

;; Movements are simple vectors showing a +ve or -ve cash movement 
;; e.g. [["M" "D"] 20] is movement of 20 from M to D

(defn normalise-movement
  "Ensure that we track both directions of movement against same key.
e.g instead of [[M P] 20] and [[P M] 10] we just have [[M P] 10]."
  [[[from to] amount :as movement]]
  (if (neg? (compare from to))
    [[to from] (- amount)]
    movement))

(defn payment->movements
  "Convert single payment in input format to seq of movements of the 
form [[from to] amount]."
  [{:keys [payer beneficiaries currency amount]}]
  (let [share (convert currency (/ amount (count beneficiaries)))]
    (remove nil?
            (for [b beneficiaries :when (not= b payer)]
              (normalise-movement [[payer b] share])))))

(defn calculate-compensating-movements
  "Turn input vector into vector of compensating movements needed to
reverse the net movements described."
  [input]
  (->> input
       (mapcat payment->movements) ; [ [["M" "P"] 30] [["M" "G"] 26] ... ]
       (map (partial apply hash-map)) ; turn each into a single element map for merging
       (reduce #(merge-with + %1 %2)) ; reduce to accumulate
       (fmap -))) ; negative for compensating payment

(defn report [movements]
  (for [[[from to] amount] movements]
    (str from " owes " to " GBP " amount)))

