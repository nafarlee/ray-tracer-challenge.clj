(ns ray.ray
  (:require
    [ray.tuple :refer [add multiply]]))

(defrecord ray [origin direction])

(defn position [{:keys [origin direction]} t]
  (->> t
       (multiply direction)
       (add origin)))
