(ns ray.ray
  (:require
    [ray.tuple :refer [add multiply]]))

(defrecord ray [origin direction])

(defn position [{:keys [origin direction]} t]
  (add origin
       (multiply direction t)))
