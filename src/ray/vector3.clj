(ns ray.vector3
  (:require
    [ray.tuple :refer [tuple tuple?]]))

(defn vector3 [x y z]
  (tuple x y z 0.0))

(defn vector3?  [[_ _ _ [w] :as t]]
  (and (tuple? t)
       (zero? w)))

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
