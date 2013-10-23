(ns zombie.core
  (:require [zombie.parallel :as parallel]
            [clojure.tools.reader.edn :as edn])
  (:gen-class))

(def chance-of-infection 0.75)
(def purposeful-movement-threshold 9)
(def num-cycles (atom 1000))
(def num-threads 5)
(def initial-population 233300)
(def population-after-exodus (/ initial-population 2))
(def mesh-width 1000)
(def mesh-length 1500)
(def population-per-square-km (/ population-after-exodus (* mesh-width mesh-length)))
(def hostname (.getHostName (java.net.InetAddress/getLocalHost)))
(def cluster-size (atom 1))
(def my-rank (atom 0))
(defrecord Mesh [rank thread x y width length population])
(defrecord Location [x y])
(defrecord Person [gender age infection-status location])

(defn has-human?
  []
  (< (rand) population-per-square-km))

(defn gender?
  []
  (if (< 0.5 (rand)) :male :female))

(defn age?
  []
  :adult)

(defn create-person
  [gender age status x y width-offset height-offset]
  (->Person gender age status (->Location (+ width-offset x) (+ height-offset y))))

(defn add-person
 [mesh row column width-offset height-offset]
 (if (has-human?) (cons (create-person (gender?) (age?) :susceptible column row width-offset height-offset) mesh) mesh))

(defn create-row
  [previous row width width-offset height-offset]
  (loop [mesh previous
         column 0]
    (if (= column width)
      mesh
      (recur (add-person mesh row column width-offset height-offset) (inc column)))))

(defn create-population
  [width height width-offset height-offset]
  (loop [mesh []
         row 0]
    (if (= row height)
      mesh
      (recur (create-row mesh row width width-offset height-offset) (inc row))))
  )

(defn create-mesh
  [rank thread num-processes num-threads-this-process]
  (let [thread-width (/ mesh-width num-threads-this-process)
        thread-length (/ mesh-length num-processes)]
    (create-population thread-width thread-length (* thread thread-width) (* rank thread-length))
    ))

(defn print-message
  [message]
  (println (str hostname " - " (parallel/processor-name) " : " message " from " @my-rank " of " @cluster-size)))

(defn zero-dec
  [num]
  (if (= 0 num) num (dec num)))

(defn inc-max
  [num limit]
  (if (= limit num)
    num
    (inc num)))

(defn move-randomly
  [location]
  (let [chance (rand)
        x (-> location :x)
        y (-> location :y)]
      (cond
       (> chance 0.8) (Location. (inc-max x mesh-width) y)
       (> chance 0.6) (Location. x (inc-max y mesh-length))
       (> chance 0.4) (Location. (zero-dec x) y)
       (> chance 0.2) (Location. x (zero-dec y))
       :else location)))

(defn move-towards
  [to from]
  (cond
   (> to from) (inc from)
   (< to from) (dec from)
   :else from))

(defn distance-between
  [to from]
  (let [
        x-diff (- (:x to) (:x from))
        y-diff (- (:y to) (:y from))]
    (+ (* x-diff x-diff) (* y-diff y-diff))))

(defn distance
  [location person]
  (distance-between location (-> person :location)))

(defn closest-person-to
  [location environment]
  (let [closest (:location (first (sort-by (partial distance location) environment)))]
    (if (> purposeful-movement-threshold (distance-between location closest)) closest (move-randomly location))))

(defn change-location-zombie
  [zombie environment]
  (let [closest-person (closest-person-to (:location zombie) environment)
        zombie-location (:location zombie)
        x (move-towards (:x closest-person) (:x zombie-location))
        y (move-towards (:y closest-person) (:y zombie-location))]
    (->Location x y)))

(defn change-location
  [person environment]
  (if (= :zombie (:infection-status person)) (change-location-zombie person environment) (move-randomly (->  person :location))))

(defn move-person
  [environment person]
  (assoc person :location (change-location person environment))
)

(defn make-zombie
  [person]
  (do (println "making a zombie")
    (assoc person :infection-status :zombie :location (move-randomly (-> person :location)))))

(defn zombies-from
  [population]
  (for [person population :when (= :zombie (:infection-status person))] person))

(defn people-from
  [population]
  (for [person population :when (= :susceptible (:infection-status person))] person))

(defn same-location
  [under-test population]
  (for [person population :when (= (:location person) (:location under-test))] person))

(defn can-be-infected
  [zombies person]
  (if (or (> chance-of-infection (rand)) (empty? (same-location person zombies))) person (assoc person :infection-status :zombie)))

(defn turn-to-zombies
  [population]
  (let [zombies (zombies-from population)]
    (map (partial can-be-infected zombies) population)))

(defn move
  [previous]
  (map (partial move-person (people-from previous)) (turn-to-zombies previous)))

(defn neighbours
  [rank upper]
  (for [x [(dec rank) (inc rank)] :when (and (>= x 0) (< x upper))] x))

(defn my-neighbours [] (neighbours @my-rank @cluster-size))

(defn read-updates
  [message]
  (binding [*read-eval* true] (read-string message)))

(defn write-updates
  [updates]
  (with-out-str (println updates)))

(defn belongs-to?
  [rank person]
  (let [process-length (/ mesh-length @cluster-size)
        start (* process-length rank)
        out-of-bound (* process-length (inc rank))
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
    (pmap (fn [neighbour]
            (parallel/send-to (write-updates (updates-for-neighbour neighbour mesh)) neighbour)) (my-neighbours))
    (loop [mine (remove-all-that-have-moved-to-a-neighbour mesh)
           neighbour-count (count (my-neighbours))]

      (if (= 0 neighbour-count)
        mine
        (recur (concat mine (read-updates (parallel/receive-from))) (dec neighbour-count))))))

(defn sort-by-x
  [mesh]
  (sort-by (fn [person] (-> person :location :x)) mesh))

(defn parallel-move
  [mesh]
  (flatten (reduce concat (pmap (fn [submesh] (move submesh)) (partition (quot (count mesh) num-threads) (sort-by-x mesh))))))

(defn stop-processing?
  [mesh]
  (println (str @my-rank " has " (count mesh) " with " (count (zombies-from mesh)) " zombies"))
  (swap! num-cycles dec)
  (= 0 @num-cycles))

(defn make-first-two-zombies
      [population]
      (let [zombie-one (make-zombie (first population))
            zombie-two (make-zombie (first (rest population)))]
      (cons zombie-one (cons zombie-two (rest (rest population))))))

(defn make-initial-zombie-population
  [population]
  (if (= 0 @my-rank) (make-first-two-zombies population) population))

(defn move-population
  [initial]
  (loop [mesh (make-initial-zombie-population initial)]
    (if (stop-processing? mesh)
      mesh
      (recur (parallel-move (swap-with-neighbours mesh))))))

(defn process-mesh
  [rank size]
  (reduce concat (pmap (fn [thread] (create-mesh rank thread size num-threads)) (range num-threads))))

(defn start-processing
  []
  (move-population (process-mesh @my-rank @cluster-size)))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (parallel/in-parallel my-rank cluster-size
   #(start-processing) (into-array String args))

)
