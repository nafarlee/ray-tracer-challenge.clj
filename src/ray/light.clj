(ns ray.light
  (:require
   [ray.color :refer [color]]))

(defrecord PointLight [position intensity])

(defn lighting [& _]
  (color 0 0 0))
