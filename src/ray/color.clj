(ns ray.color
  (:require
    [ray.tuple :refer [tuple tuple?]]))

(defn color [r g b]
  (tuple r g b))

(def color? tuple?)

(def black (color 0 0 0))
