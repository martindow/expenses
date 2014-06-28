(ns expenses.simples)

(defn lookup-rate [from-currency to-currency]
  (if (= from-currency to-currency)
    1.0
    (if (and (= from-currency "GBP") (= to-currency "PLN"))
      5.0
      (/ 1 (lookup-rate to-currency from-currency)))))

(defn convert [from-currency amount to-currency]
  (* amount (lookup-rate from-currency to-currency)))

(defn explode [payment]
  (let [beneficiaries (:beneficiaries payment)
        owing-beneficiaries (remove #(= (:payer payment) %) beneficiaries)
        count-beneficiaries (count beneficiaries)]
    (map (fn [beneficiary]
           (-> payment
               (dissoc :beneficiaries)
               (assoc :beneficiary beneficiary)
               (assoc :amount (/ (convert (:currency payment) (:amount payment) "GBP") count-beneficiaries))
               (assoc :currency "GBP")))
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
  (remove #(= 0.0 (:amount %))
          (map second
               (reduce payment-transaction-reducer {} (flatten (map explode payments))))))

(defn sentencize [txns]
  (reduce #(str %1 (:beneficiary %2) " owes " (:payer %2) " " (:currency %2) (:amount %2) "\n") "" txns))

(defn report-expenses [payments]
  (sentencize (reduce-transactions payments)))

(defmacro expenses [& xs]
  (let [rows (partition 6 xs)
        build-payment (fn [[label payer _ money-sym _ beneficiaries]]
                        (let [money (str money-sym)
                              currency (subs money 0 3)
                              amount (Double/parseDouble (subs money 3))]
                          {:payer (str payer)
                           :beneficiaries (vec (map str beneficiaries))
                           :currency currency
                           :amount amount
                           :label label}))
        input (map build-payment rows)]
    (list 'report-expenses (vec input))))

(println (expenses
          "Accomodation" M -> GBP278 -> [M P D L G]
          "Taxi" L -> PLN100 -> [M D L G]
          "Beer & Pizza" M -> PLN150 -> [M G D L]
          "Coffee" G -> PLN26 -> [G D L]
          "Beer for home" L -> PLN32 -> [M D L G]
          "Supermarket" M -> PLN40 -> [M D L G P]
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
