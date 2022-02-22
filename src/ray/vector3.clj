(ns ray.vector3
  (:require
    [ray.matrix :as m]
    [ray.tuple :refer [dot tuple tuple?]]))

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

(defn reflect [in normal]
  {:pre  [(vector3? in)
          (vector3? normal)]
   :post [(vector3? %)]}
  (->> (dot in normal)
       (* 2)
       (m/scalar-multiply normal)
       (m/subtract in)))
