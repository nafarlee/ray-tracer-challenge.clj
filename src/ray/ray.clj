(ns ray.ray
  (:require
    [ray.matrix :as m]))

(defn ray [origin direction]
  (mapv (comp vec concat) origin direction))

(defn origin [r]
  (mapv (comp vector #(nth % 0)) r))

(defn direction [r]
  (mapv (comp vector second) r))

(defn position [r t]
  (->> t
       (m/scalar-multiply (direction r))
       (m/add (origin r))))

(defn transform [r m]
  (m/multiply m r))
