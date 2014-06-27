;; ---------------------------
;; Krakow Euroclojure Expenses
;; ---------------------------

(expenses
 M -> GBP278 -> [M,P,D,L,G]
 M -> PLN150 -> [M,G,D,L]
 G -> PLN26 -> [G,D,L])

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

["Martin owes Greg GBP27.83"
 "Dan owes Lloyd GBP36.87"]
