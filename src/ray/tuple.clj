(ns ray.tuple
  (:require
    [clojure.spec.alpha :as s]
    [ray.matrix :as m]
    [ray.math :refer [float= square]]))

(defn tuple [& ds]
  (mapv (comp vector double) ds))

(def tuple?
  (partial s/valid?
           (s/coll-of (s/tuple double?) :kind vector?)))

(defn point3 [x y z]
  (tuple x y z 1.0))

(defn point3?  [[_ _ _ [w] :as t]]
  (and (tuple? t)
       (== 1.0 w)))

(defn vector3 [x y z]
  (tuple x y z 0.0))

(defn vector3?  [[_ _ _ [w] :as t]]
  (and (tuple? t)
       (zero? w)))

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
  (vector3 (- (* ay bz)
              (* az by))
           (- (* az bx)
              (* ax bz))
           (- (* ax by)
              (* ay bx))))

(defn dot [a b]
  (->> (m/hadamard a b)
       flatten
       (apply +)))
