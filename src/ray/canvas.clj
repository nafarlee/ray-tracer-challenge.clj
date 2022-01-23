(ns ray.canvas
  (:require
    [ray.color :refer [color]]))

(defrecord Canvas [pixels width height])

(defn canvas [w h]
  (-> (* w h)
      (repeat (color 0 0 0))
      vec
      (->Canvas w h)))

(defn write-pixel [{w :width h :height ps :pixels} x y color]
  (-> ps
      (assoc (+ x (* y w)) color)
      (->Canvas w h)))

(defn pixel-at [{w :width ps :pixels} x y]
  (nth ps (+ x (* y w))))
