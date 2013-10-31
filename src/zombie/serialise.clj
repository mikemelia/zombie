(ns zombie.serialise)

(defn read-messages [message]
  (binding [*read-eval* true] (read-string message)))

(defn write [objects]
  (with-out-str (println objects)))
