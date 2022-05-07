(ns ray.world)

(defrecord World [objects light])

(defn world []
  (->World [] nil))
