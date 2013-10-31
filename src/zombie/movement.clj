(ns zombie.movement
  (:use [zombie.maths]
        [zombie.data]
        [zombie.dimensions]))

(defn move-randomly
  [location]
  (let [chance (rand)
        x (-> location :x)
        y (-> location :y)]
      (cond
       (> chance 0.9) (create-location (inc-max x mesh-width) y)
       (> chance 0.8) (create-location x (inc-max y mesh-length))
       (> chance 0.7) (create-location (zero-dec x) y)
       (> chance 0.6) (create-location x (zero-dec y))
       :else location)))

(defn move-towards
  [to from]
  (cond
   (> to from) (inc from)
   (< to from) (dec from)
   :else from))

(defn move-away
  [to from]
  (cond
   (> to from) (inc from)
   (< to from) (dec from)
   :else from))
