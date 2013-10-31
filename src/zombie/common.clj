(ns zombie.common)
;; the number of threads we will use for concurrency
(def num-threads 10)
;; used to store the cluster size (i.e. number of participating processes)
(def cluster-size (atom 1))
;; my rank within the cluster
(def my-rank (atom 0))
;; real population stats
;;(def initial-population 233300)
;;(def mesh-width 1000)
;;(def mesh-length 1500)
;; cut down population facts
(def initial-population (* 4 2333))
(def mesh-width 200)
(def mesh-length 300)

(defn locations-from [population]
  (map (fn [person] (-> person :location)) population))

(defn filter-age [population age]
  (filter (fn [person] (= age (:age person))) population))

(defn zombies-from [population]
  (for [person population :when (= :zombie (:infection-status person))] person))

(defn people-from [population]
  (for [person population :when (= :susceptible (:infection-status person))] person))

(defn distance-between [to from]
  (let [x-diff (- (:x to) (:x from))
        y-diff (- (:y to) (:y from))]
    (+ (* x-diff x-diff) (* y-diff y-diff))))

(defn how-far-from [location person]
  (distance-between location (:location person)))
