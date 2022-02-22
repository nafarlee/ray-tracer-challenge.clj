(ns ray.material
  (:require
   [ray.color :refer [color]]))

(defrecord Material [color ambient diffuse specular shininess])

(defn material []
  (->Material
   (color 1 1 1)
   0.1
   0.9
   0.9
   200.0))