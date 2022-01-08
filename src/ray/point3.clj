(ns ray.point3
  (:require
    [ray.tuple :refer [tuple tuple?]]))

(defn point3 [x y z]
  (tuple x y z 1.0))

(defn point3?  [[_ _ _ [w] :as t]]
  (and (tuple? t)
       (== 1.0 w)))

