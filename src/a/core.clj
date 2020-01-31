(ns a.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defrecord Tuple [x y z w])

(defn point [x y z]
  (->Tuple x y z 1))

(defn point? [x]
  (and (instance? Tuple x)
       (= 1.0 (:w x))))

(defn vector' [x y z]
  (->Tuple x y z 0))

(defn vector'? [x]
  (and (instance? Tuple x)
       (zero? (:w x))))
