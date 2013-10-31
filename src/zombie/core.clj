;; main processing loop
(ns zombie.core
  (:use [zombie.parallel]
        [zombie.initial]
        [zombie.dimensions]
        [zombie.worker]
        [zombie.data]
        [zombie.master])
  (:gen-class))

(defn start-processing []
  (if (> @my-rank 0)
    (move-population (sort-people (process-mesh (dec @my-rank) (dec @cluster-size)) y-coordinate))
    (monitor-state)))

(defn -main [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (in-parallel my-rank cluster-size start-processing (into-array String args))

)
