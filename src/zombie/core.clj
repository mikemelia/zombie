(ns zombie.core
  (:use [zombie.parallel]
        [zombie.common]
        [zombie.initial]
        [zombie.data]
        [zombie.lifecycle]
        [zombie.maths]
        [zombie.serialise]
        [zombie.master])
  (:gen-class))

;; chance that a single zombie can infect an encountered human
(def chance-of-infection 0.5)
;; the chance that a human can kill a zombie
(def chance-of-killing {:child 0.01 :adult 0.2 :elderly 0.05})
;; if we are to move with purpose (i.e towards a nearby person/zombie) then the maximum distance to the target before we wander randomly
(def purposeful-movement-threshold 32)
(def hostname (.getHostName (java.net.InetAddress/getLocalHost)))
;; used for the initial Mesh population creation
(defrecord Mesh [rank thread x y width length population])

(defn print-message
  [message]
  (println (str hostname " - " (processor-name) " : " message " from " @my-rank " of " @cluster-size)))

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

(defn close-enough
  [a b]
  (>= purposeful-movement-threshold (distance-between a b)))

(defn someone-nearby [location close-people]
  (rand-nth (sort-by (fn [person] (how-far-from location person)) close-people)))

(defn closest-to
  [location environment]
  (let [close-people (filter (fn [close-person] (close-enough location (:location close-person))) environment)]
    (if (empty? close-people)
      (move-randomly location)
      (:location (someone-nearby location close-people)))))

(defn change-location-sensible
  [subject environment movement-function ability-to-move-with-direction]
  (let [closest (closest-to (:location subject) environment)
        location (:location subject)
        x (if (> ability-to-move-with-direction (rand)) (movement-function (:x closest) (:x location)) (:x location))
        y (if (> ability-to-move-with-direction (rand)) (movement-function (:y closest) (:y location)) (:y location))]
    (create-location x y)))

(defn change-location
  [person environment]
  (cond
   (= :zombie (:infection-status person)) (change-location-sensible person (people-from environment) move-towards 1.0)
   (= :adult (:age person)) (change-location-sensible person (zombies-from environment) move-away 0.75)
   (= :child (:age person)) (change-location-sensible person (people-from environment) move-towards 0.5)
   :else (move-randomly (->  person :location))))

(defn sort-people
  [people comparator]
  (sort-by (fn [person] (comparator person)) people))

(defn move-person
  [environment person]
  (assoc person :location (change-location person environment))
)

(defn make-zombie
  [person]
  (do (println "making a zombie")
    (assoc person :infection-status :zombie :location (move-randomly (-> person :location)))))

(defn same-location
  [under-test population]
  (for [person population :when (= (:location person) (:location under-test))] person))

(defn survival [chance status person]
  (if (> (rand) chance) person (assoc person :infection-status status)))

(defn combined-strength [people]
  (/ (reduce + (map (fn [person] ((:age person) chance-of-killing)) people)) (count people)))

(defn fight [zombies people]
  (let [num-zombies (count zombies)
        num-people (count people)]
    (concat (map (partial survival (/ (* num-zombies chance-of-infection) num-people) :zombie) people) (map (partial survival (/ (combined-strength people) num-zombies) :really-dead) zombies))))

(defn interaction [same-location]
  (let [zombies (zombies-from same-location)
        people (people-from same-location)]
    (cond
     (= 1 (count same-location)) same-location
     (= 0 (count zombies)) people
     (= 0 (count people)) zombies
     :else (fight zombies people))))

(defn handle-interaction [population]
  (let [same-location (group-by :location population)]
    (reduce concat (map interaction (vals same-location)))))

(defn move
  [previous]
  (map (partial move-person previous) (handle-interaction previous)))

(defn neighbours
  [rank upper]
  (for [y [(dec rank) (inc rank)] :when (and (> y 0) (< y upper))] y))

(defn my-neighbours [] (neighbours @my-rank @cluster-size))

(defn belongs-to?
  [rank person]
  (let [process-length (inc (quot mesh-length (dec @cluster-size)))
        start (* process-length (dec rank))
        out-of-bound (* process-length rank)
        y-coordinate (-> person :location :y)]
    (and (>= y-coordinate start) (< y-coordinate out-of-bound))))

(defn belongs-to-neighbour?
  [neighbour-rank person]
  (belongs-to? neighbour-rank person))


(defn belongs-to-me?
  [person]
  (belongs-to? @my-rank person))

(defn updates-for-neighbour
  [neighbour mesh]
  (for [person mesh :when (belongs-to-neighbour? neighbour person)] person))

(defn remove-all-that-have-moved-to-a-neighbour
  [mesh]
  (for [person mesh :when (belongs-to-me? person)] person))

(defn swap-with-neighbours
  [mesh]
  (do
    (send-to (write mesh) 0)
    (doseq [neighbour (my-neighbours)]
            (send-to (write (updates-for-neighbour neighbour mesh)) neighbour))
    (loop [mine (remove-all-that-have-moved-to-a-neighbour mesh)
           neighbour-count (count (my-neighbours))]
      (if (= 0 neighbour-count)
        mine
        (recur (concat mine (read-messages (receive-from))) (dec neighbour-count))))))


(defn parallel-move
  [mesh]
  (reduce concat (pmap (fn [submesh] (move submesh)) (partition-all (inc (quot (count mesh) num-threads)) (sort-people mesh x-coordinate)))))

(defn make-first-two-zombies
      [population]
      (let [zombie-one (make-zombie (first population))
            zombie-two (make-zombie ( second population))]
      (cons zombie-one (cons zombie-two (rest (rest population))))))

(defn make-initial-zombie-population
  [population]
  (if (= 1 @my-rank) (make-first-two-zombies population) population))

(defn move-population
  [initial]
  (loop [mesh (make-initial-zombie-population initial)]
    (if (stop-processing? mesh)
      mesh
      (recur (parallel-move (swap-with-neighbours mesh))))))

(defn process-mesh [rank size]
  (reduce concat (pmap (fn [thread] (create-mesh rank (dec thread) size num-threads)) (range num-threads))))

(defn start-processing
  []
  (if (> @my-rank 0)
    (move-population (sort-people (process-mesh (dec @my-rank) (dec @cluster-size)) y-coordinate))
    (monitor-state)))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (in-parallel my-rank cluster-size start-processing (into-array String args))

)
