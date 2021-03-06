;; parameters for the application
(ns zombie.dimensions)
;; real population stats
;;(def initial-population 233300)
;;(def mesh-width 1000)
;;(def mesh-length 1500)
;; cut down population facts
(def downsize-factor 4)
(def initial-population (quot 233300 (* downsize-factor downsize-factor)))
(def mesh-width (quot 1000 downsize-factor))
(def mesh-length (quot 1500 downsize-factor))
(def population-after-exodus (/ initial-population 2))
(def population-per-square-km (/ population-after-exodus (* mesh-width mesh-length)))
;; used to store the cluster size (i.e. number of participating processes)
(def cluster-size (atom 1))
;; my rank within the cluster
(def my-rank (atom 0))
;; the number of threads we will use for concurrency
(def num-threads 10)
