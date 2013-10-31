(ns zombie.initial
  (:use [zombie.common]
        [zombie.data]))

(def population-after-exodus (/ initial-population 2))
(def population-per-square-km (/ population-after-exodus (* mesh-width mesh-length)))

;;
;; initial mesh creation
;;
(defn gender?
  []
  (if (< 0.5 (rand)) :male :female))

(defn age?
  []
  (cond
   (> (rand) 0.75) :child
   (> (rand) 0.25) :adult
   :else :elderly))

(defn has-human? []
  (< (rand) population-per-square-km))

(defn add-person [mesh row column width-offset height-offset]
 (if (has-human?) (cons (create-person (gender?) (age?) :susceptible column row width-offset height-offset) mesh) mesh))

(defn create-row [previous row width width-offset height-offset]
  (loop [mesh previous
         column 0]
    (if (= column width)
      mesh
      (recur (add-person mesh row column width-offset height-offset) (inc column)))))

(defn create-population [width height width-offset height-offset]
  (loop [mesh []
         row 0]
    (if (= row height)
      mesh
      (recur (create-row mesh row width width-offset height-offset) (inc row)))))

(defn create-mesh [rank thread num-processes num-threads-this-process]
  (let [thread-width (inc (quot mesh-width num-threads-this-process))
        thread-length (inc (quot mesh-length num-processes))
        width-start (* thread thread-width)
        length-start (* rank thread-length)
        real-width (if (= (inc thread) num-threads-this-process) (- mesh-width width-start) thread-width)
        real-length (if (= (inc rank) num-processes) (- mesh-length length-start) thread-length)]
    (create-population real-width real-length width-start length-start)))
