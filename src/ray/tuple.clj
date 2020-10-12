(ns ray.tuple
  (:require
    [clojure.spec.alpha :as s]
    [ray.math :refer [float= square]]))

(defn map-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(def zip (partial map vector))

(s/def ::x float?)
(s/def ::y float?)
(s/def ::z float?)
(s/def ::w #{1.0 0.0})
(s/def ::tuple (s/keys :req [::x ::y ::z ::w]))
(defn tuple [x y z w]
  {::x x, ::y y, ::z z, ::w w})

(def tuple? (partial s/valid? ::tuple))

(defn multiply [t x]
  (map-values (partial * x) t))

(defn divide [t x]
  (map-values #(/ % x) t))

(defn point [x y z]
  (tuple x y z 1))

(defn point?  [{w ::w :as t}]
  (and (tuple? t)
       (== 1.0 w)))

(defn vector' [x y z]
  (tuple x y z 0))

(defn vector'?  [{w ::w :as t}]
  (and (tuple? t)
       (zero? w)))

(def add (partial merge-with +))

(def subtract (partial merge-with -))

(def negate (partial subtract (vector' 0 0 0)))

(defn magnitude [v]
  (->> (vals v)
       (map square)
       (apply +)
       Math/sqrt))

(defn normalize [v]
  (let [m (magnitude v)]
    (->> ((juxt ::x ::y ::z) v)
         (map #(/ % m))
         (apply vector'))))

(defn dot [a b]
  (->> (merge-with * a b)
       vals
       (apply +)))

(defn cross [{ax ::x ay ::y az ::z} {bx ::x by ::y bz ::z}]
  (vector' (- (* ay bz)
              (* az by))
           (- (* az bx)
              (* ax bz))
           (- (* ax by)
              (* ay bx))))

(def hadamard (partial merge-with *))

(defn eq [a b]
  (every? (partial apply float=)
          (zip (vals a) (vals b))))
