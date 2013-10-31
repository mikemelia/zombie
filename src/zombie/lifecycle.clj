(ns zombie.lifecycle)
;; number of processing cycles (each cycle simulates an hour)
(def max-cycles 1000)
(def num-cycles (atom 0))

(defn stop-processing?
  [mesh]
  (swap! num-cycles inc)
  (< max-cycles @num-cycles))
