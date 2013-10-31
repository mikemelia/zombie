;; represents the processing performed by the master process
(ns zombie.master
  (:use [zombie.parallel]
        [zombie.visualise]
        [zombie.data]
        [zombie.dimensions]
        [zombie.serialise]
        [zombie.lifecycle]))

;; ensure we have enough of a buffer for inter-process comms
(def person-size (count (with-out-str (println (create-person :female :adult :susceptible mesh-width mesh-length 0 0)))))
(defn max-buffer-size [] (+ 100 (/ (* (+ 20 person-size) initial-population) (dec @cluster-size))))

(defn get-state []
  (reduce concat (map (fn [rank] (read-messages (receive-from (max-buffer-size)))) (take (dec @cluster-size) (range 1 @cluster-size)))))

(defn split-infected-responses
  []
  (let [state (get-state)
        people (people-from state)
        zombies (zombies-from state)
        adults (filter-age people :adult)
        children (filter-age people :child)
        elderly (filter-age people :elderly)]
    [(locations-from adults) (locations-from children) (locations-from elderly) (locations-from zombies)]))

(defn monitor-state []
  (let [frame (create-frame mesh-width mesh-length downsize-factor)]
    (loop [[adults children elderly zombies] (split-infected-responses)
           time 1]
      (if (= max-cycles time)
        (.dispose frame)
        (do
          (draw-population frame adults children elderly zombies time)
          (recur (split-infected-responses) (inc time))))))
  )
