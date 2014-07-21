(ns expenses.simpler
  (:require [clojure.algo.generic.functor :refer [fmap]]))

(defmacro expenses
  "Just convert to appropriate data structure, process with fns."
  [& xs]
  (vec (for [[label payer _ value _ beneficiaries] (partition 6 xs)]
         (let [[_ currency amount] (first (re-seq #"(\D{3})(\d+)" (name value)))]
           {:label label
            :payer (str payer)
            :currency currency
            :amount (Double/parseDouble amount)
            :beneficiaries (vec (map str beneficiaries))}))))

(defn convert
  "Convert everything to GBP."
  [currency amount]
  (case currency
    "PLN" (/ amount 5)
    "GBP" amount))

;; Payments are map with keys - :payer, :beneficiaries, :currency, :amount

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

(defn report
  "Generate human readable description of seq of required cash movements."
  [movements]
  (sort
   (for [[[from to] amount] movements]
     (if (pos? amount)
       (str from " owes " to " GBP " (format "%.2f" amount))
       (str to " owes " from " GBP " (format "%.2f" (- amount)))))))

(comment
  (def tally (expenses
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
              "Shopping" L -> PLN29 -> [M P D L]))

  (report (calculate-compensating-movements tally)))
