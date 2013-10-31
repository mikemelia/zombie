(ns zombie.maths)

(defn zero-dec [number-to-decrement]
  (if (= 0 number-to-decrement)
    number-to-decrement
    (dec number-to-decrement)))

(defn inc-max [number-to-increment limit]
  (if (= limit number-to-increment)
    number-to-increment
    (inc number-to-increment)))
