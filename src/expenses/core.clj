(ns expenses.core)

;; ---------------------------
;; Krakow Euroclojure Expenses
;; ---------------------------

(expenses
 "Accomodation": M -> GBP278 -> [M P D L G]
 "Taxi": L -> PLN100 -> [M D L G]
 "Beer & Pizza": M -> PLN150 -> [M G D L]
 "Coffee": G -> PLN26 -> [G D L]
 "Beer for home": L -> PLN32 -> [M D L G]
 "Supermaket": M -> PLN40 -> [M D L G P]
 "Dinner": D -> PLN200 -> [M D L]
 "Dinner": D -> PLN200 -> [M D L G]
 "Dinner": M -> PLN20 -> [M D L G]
 "Beer": G -> PLN70 -> [M D L G P]
 )

;; ---------------------------
;; M: Martin
;; L: Lloyd
;; G: Greg
;; D: Dan
;; P: Paddy
;; ---------------------------

(process
 [{:payer "M"
   :beneficiaries ["M" "P" "L" "G" "D"]
   :currency "GBP"
   :amount 278}
  {:payer "G"
   :beneficiaries ["G" "D" "L"]
   :currency "PLN"
   :amount 26}])

(process-expanded
 [{:payer "M"
   :beneficiary "M"
   :currency "GBP"
   :amount 55.6}
  {:payer "M"
   :beneficiary "M"
   :currency "GBP"
   :amount 55.6}
  {:payer "M"
   :beneficiary "M"
   :currency "GBP"
   :amount 55.6}
  {:payer "M"
   :beneficiary "M"
   :currency "GBP"
   :amount 55.6}
  {:payer "M"
   :beneficiary "M"
   :currency "GBP"
   :amount 55.6}
  {:payer "G"
   :beneficiary "G"
   :currency "PLN"
   :amount 26.66667}
  {:payer "G"
   :beneficiary "D"
   :currency "PLN"
   :amount 26.66667}
  {:payer "G"
   :beneficiary "L"
   :currency "PLN"
   :amount 26.66667}])

;;
;;
;;




(defn accumulate-amounts [payments role]
  (into {}
        (map
         (fn [[k v]] [k (apply + (map :amount v))])
         (group-by role payments))))

(def paid (thing-by payments :payer))
(def benefited (thing-by payments :beneficiary))

(defn spend [payments]
  (apply +
   (map :amount
        (filter #(= payer (:payer %))  payments))))


[{G [:paid 234 :received] 67567
  D []}]


(defn benefit [payments beneficiary]
  (apply +
   (map :amount
        (filter #(= beneficiary (:beneficiary %))  payments))))

["Martin owes Greg GBP27.83"
 "Dan owes Lloyd GBP36.87"]
