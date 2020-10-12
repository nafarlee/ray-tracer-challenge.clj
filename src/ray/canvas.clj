(ns ray.canvas
  (:require
    [clojure.spec.alpha :as s]
    [ray.color :refer [color]]))

(s/def ::pixels vector?)
(s/def ::width (s/and pos? integer?))
(s/def ::height (s/and pos? integer?))
(s/def ::canvas (s/keys :req [::pixels ::width ::height]))

(defn ->canvas [p w h]
  {::pixels p, ::width w, ::height h})

(defn canvas [w h]
  (-> (* w h)
      (repeat (color 0 0 0))
      vec
      (->canvas w h)))

(defn write-pixel [{w ::width h ::height ps ::pixels} x y color]
  (-> ps
      (assoc (+ x (* y w)) color)
      (->canvas w h)))

(defn pixel-at [{w ::width ps ::pixels} x y]
  (nth ps (+ x (* y w))))
