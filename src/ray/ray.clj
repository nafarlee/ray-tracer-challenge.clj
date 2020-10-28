(ns ray.ray
  (:require
    [ray.matrix :as m]
    [ray.tuple :refer [add multiply]]))

(defn ray [origin direction]
  (mapv (comp vec concat) origin direction))

(defn origin [r]
  (mapv (comp vector first) r))

(defn direction [r]
  (mapv (comp vector second) r))

(defn position [r t]
  (->> t
       (multiply (direction r))
       (add (origin r))))

(defn transform [r m]
  (m/multiply m r))
