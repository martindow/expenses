(ns expenses.simples)

(def input
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

(defn explode [payment]
  (let [beneficiaries (:beneficiaries payment)
        owing-beneficiaries (remove #(= (:payer payment) %) beneficiaries)
        count-beneficiaries (count beneficiaries)]
    (map (fn [beneficiary]
           (assoc
               (assoc (dissoc payment :beneficiaries) :beneficiary beneficiary)
             :amount (/ (:amount payment) count-beneficiaries)))
         owing-beneficiaries)))

(defn update-balance [balance-accumulator payment]
  (if (= (:beneficiary balance-accumulator) (:beneficiary payment))
    (update-in balance-accumulator [:amount] #(+ % (:amount payment)))
    (update-in balance-accumulator [:amount] #(- % (:amount payment)))))

(defn payment-transaction-reducer [accumulator payment]
  (let [relationship (set [(:beneficiary payment) (:payer payment)]) ;; #{D, M}
        balance-accumulator (accumulator relationship)]
    (if balance-accumulator
      (assoc accumulator relationship (update-balance balance-accumulator payment))
      (assoc accumulator relationship payment))))

(defn reduce-transactions [payments]
  (remove #(= 0 (:amount %))
          (map second
               (reduce payment-transaction-reducer {} (flatten (map explode payments))))))

(defn sentencize [txns]
  (reduce #(str %1 (:beneficiary %2) " owes " (:payer %2) " " (:amount %2) (:currency %2) "\n") "" txns))

(defn report-expenses [payments]
  (sentencize (reduce-transactions payments)))
