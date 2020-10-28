(ns ray.ray
  (:require
    [ray.tuple :refer [add multiply]]))

(defn ray [origin direction]
  (mapv (comp vec concat) origin direction))

(defn position [{:keys [origin direction]} t]
  (->> t
       (multiply direction)
       (add origin)))
