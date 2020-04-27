(ns a.core
  (:require [clojure.pprint :refer (pprint)])
  (:gen-class))

(defprotocol Multiply (multiply [this scalar]))

(defprotocol Divide (divide [this scalar]))

(def EPSILON 0.00001)

(defn float=
  [a b]
  (->> a
       (- b)
       Math/abs
       (> EPSILON)))

(defn map-values
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn element-wise
  [from-map f m i]
  (->> m
       (map-values #(f % i))
       from-map))

(def zip (partial map vector))

(defn eq
  [a b]
  (every? (partial apply float=)
          (zip (vals a) (vals b))))

(defrecord Tuple [x y z w]
  Multiply
  (multiply [this scalar]
    (element-wise map->Tuple * this scalar))
  Divide
  (divide [this scalar]
    (element-wise map->Tuple / this scalar)))

(defn point [x y z]
  (->Tuple x y z 1))

(defn point? [{w :w :as t}]
  (and (instance? Tuple t)
       (== 1.0 w)))

(defn vector' [x y z]
  (->Tuple x y z 0))

(defn vector'? [{w :w :as t}]
  (and (instance? Tuple t)
       (zero? w)))

(defrecord Color [red green blue]
  Multiply
  (multiply [this scalar]
    (element-wise map->Color * this scalar))
  Divide
  (divide [this scalar]
    (element-wise map->Color / this scalar)))

(def add (partial merge-with +))

(def subtract (partial merge-with -))

(def negate (partial subtract (vector' 0 0 0)))

(defn magnitude [v]
  (Math/sqrt (+ (Math/pow (:x v) 2)
                (Math/pow (:y v) 2)
                (Math/pow (:z v) 2))))

(defn normalize
  [v]
  (let [m (magnitude v)]
    (->> ((juxt :x :y :z) v)
         (map #(/ % m))
         (apply vector'))))

(defn dot [a b]
  (->> (merge-with * a b)
       vals
       (apply +)))

(defn cross
  [{ax :x ay :y az :z} {bx :x by :y bz :z}]
  (vector' (- (* ay bz)
              (* az by))
           (- (* az bx)
              (* ax bz))
           (- (* ax by)
              (* ay bx))))

(def hadamard (partial merge-with *))

(defrecord Projectile [position velocity])

(defrecord Environment [gravity wind])

(defn tick
  [{g :gravity w :wind} {p :position v :velocity}]
  (let [position (add p v)
        velocity (reduce add [v g w])]
    (->Projectile position velocity)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [gravity (vector' 0 -0.1 0)
        wind (vector' -0.01 0 0)
        environment (->Environment gravity wind)
        position (point 0 1 0)
        velocity (vector' 1 1 0)
        initial (->Projectile position velocity)
        f (partial tick environment)]
    (->> initial
         (iterate f)
         (take-while (comp (partial <= 0) :y :position))
         pprint)))
