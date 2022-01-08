(ns ray.tuple
  (:require
    [clojure.spec.alpha :as s]
    [ray.matrix :as m]
    [ray.math :refer [float= square]]))

(s/def ::tuple (s/tuple (s/tuple double?)
                        (s/tuple double?)
                        (s/tuple double?)
                        (s/tuple #{1.0 0.0})))
(defn tuple [x y z w]
  [[(double x)]
   [(double y)]
   [(double z)]
   [(double w)]])

(def tuple? (partial s/valid? ::tuple))

(defn point [x y z]
  (tuple x y z 1.0))

(defn point?  [[_ _ _ [w] :as t]]
  (and (tuple? t)
       (== 1.0 w)))

(defn vector' [x y z]
  (tuple x y z 0.0))

(defn vector'?  [[_ _ _ [w] :as t]]
  (and (tuple? t)
       (zero? w)))

(def add (partial m/entrywise +))

(def subtract (partial m/entrywise -))

(defn negate [t]
  (m/fmap (fn [e _ _] (- e)) t))

(defn magnitude [v]
  (->> v
       flatten
       (mapv square)
       (apply +)
       Math/sqrt))

(defn normalize [v]
  (let [m (magnitude v)]
     (m/fmap (fn [e _ _] (/ e m)) v)))

(defn cross [[[ax]
              [ay]
              [az]]
             [[bx]
              [by]
              [bz]]]
  (vector' (- (* ay bz)
              (* az by))
           (- (* az bx)
              (* ax bz))
           (- (* ax by)
              (* ay bx))))

(def hadamard (partial m/entrywise *))

(defn dot [a b]
  (->> (hadamard a b)
       flatten
       (apply +)))
