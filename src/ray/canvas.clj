(ns ray.canvas
  (:require
    [ray.color :refer [color]]))

(defn ->canvas [p w h]
  {:pixels p, :width w, :height h})

(defn canvas [w h]
  (-> (* w h)
      (repeat (color 0 0 0))
      vec
      (->canvas w h)))

(defn write-pixel [{w :width h :height ps :pixels} x y color]
  (-> ps
      (assoc (+ x (* y w)) color)
      (->canvas w h)))

(defn pixel-at [{w :width ps :pixels} x y]
  (nth ps (+ x (* y w))))
