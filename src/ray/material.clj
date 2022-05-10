(ns ray.material
  (:require
   [ray.color :refer [color]]))

(defrecord Material [color ambient diffuse specular shininess])

(def Material? (partial instance? Material))

(defn material
  ([] (material {}))
  ([& {:as opts}]
   (map->Material
    (merge
     {:color     (color 1 1 1)
      :ambient   0.1
      :diffuse   0.9
      :specular  0.9
      :shininess 200.0}
     opts))))
