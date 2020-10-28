(ns ray.ray
  (:require
    [ray.matrix :as m]
    [ray.tuple :refer [add multiply]]))

(defn ray [origin direction]
  (mapv (comp vec concat) origin direction))

(defn position [{:keys [origin direction]} t]
  (->> t
       (multiply direction)
       (add origin)))

(defn transform [r m]
  (m/multiply m r))
