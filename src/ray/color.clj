(ns ray.color
  (:require
    [ray.tuple :refer [tuple]]))

(defn color [r g b]
  (tuple r g b))

(def black (color 0 0 0))
