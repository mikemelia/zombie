(ns zombie.core
  (:gen-class))
(require '[zombie.parallel :as parallel])

(def hostname (.getHostName (java.net.InetAddress/getLocalHost)))

(defn print-message
  [message]
  (println (str hostname " - " (parallel/processor-name) " : " message " from " (parallel/my-rank) " of " (parallel/size))))
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (parallel/in-parallel
   #(print-message "Hello World") (into-array String args))
  (print-message "Hello World")

)
