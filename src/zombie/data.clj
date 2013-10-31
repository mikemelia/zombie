(ns zombie.data)
;; the location coordinates
(defrecord Location [x y])
;; represents a person or a zombie
(defrecord Person [gender age infection-status location])

(defn y-coordinate [person]
  (-> person (-> :location :y)))

(defn x-coordinate [person]
  (-> person (-> :location :x)))

(defn create-location [x y]
  (Location. x y))

(defn create-person [gender age status x y width-offset height-offset]
  (Person. gender age status (create-location (+ width-offset x) (+ height-offset y))))

(defn zombies-from [population]
  (for [person population :when (= :zombie (:infection-status person))] person))

(defn people-from [population]
  (for [person population :when (= :susceptible (:infection-status person))] person))

(defn locations-from [population]
  (map (fn [person] (-> person :location)) population))

(defn filter-age [population age]
  (filter (fn [person] (= age (:age person))) population))
