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

(defn add [t1 t2]
  (->Tuple (+ (:x t1) (:x t2))
           (+ (:y t1) (:y t2))
           (+ (:z t1) (:z t2))
           (+ (:w t1) (:w t2))))

(defn subtract [t1 t2]
  (->Tuple (- (:x t1) (:x t2))
           (- (:y t1) (:y t2))
           (- (:z t1) (:z t2))
           (- (:w t1) (:w t2))))

(defn negate [t]
  (subtract (vector' 0 0 0) t))

(defn product [t x]
  (->Tuple (* (:x t) x)
           (* (:y t) x)
           (* (:z t) x)
           (* (:w t) x)))

(defn divide [t x]
  (->Tuple (/ (:x t) x)
           (/ (:y t) x)
           (/ (:z t) x)
           (/ (:w t) x)))

(defn magnitude [v]
  (Math/sqrt (+ (Math/pow (:x v) 2)
                (Math/pow (:y v) 2)
                (Math/pow (:z v) 2))))
