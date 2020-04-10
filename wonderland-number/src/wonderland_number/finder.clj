(ns wonderland-number.finder)

(def ^:const multis (range 2 7))

(defn digitset [n]
  (-> n str set))

(defn multiples [n]
  (for [m multis] (* n m)))

(defn check [n]
  (let [dign (digitset n)
        digms (for [m (multiples n)] (digitset m))]
    (every? #(= % dign) digms)))

(defn wonderland-number []
  (let [upper (int (/ 1000000 6))]
    (loop [n upper]
      (if (check n) n (recur (- n 1))))))
