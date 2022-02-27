(ns ray.light
  (:require
   [ray.color :refer [color]]))

(defrecord PointLight [position intensity])

(def PointLight? (partial instance? PointLight))

(defn lighting [& _]
  (color 0 0 0))
