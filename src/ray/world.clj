(ns ray.world
  (:require
   [ray.point3 :refer [point3]]
   [ray.light :refer [->PointLight]]
   [ray.matrix :refer [scaling]]
   [ray.color :refer [color]]
   [ray.material :refer [material]]
   [ray.shape :refer [sphere intersect]]))

(defrecord World [objects light])

(defn world []
  (->World [] nil))

(defn default-world []
  (->World
   [(sphere
     :material
     (material :color (color 0.8 1.0 0.6) :diffuse  0.7 :specular 0.2))
    (sphere :transform (scaling 0.5 0.5 0.5))]
   (->PointLight
    (point3 -10 -10 -10)
    (color 1 1 1))))

(defn intersect-world [w r]
  (->> w
       :objects
       (map #(intersect % r))
       flatten
       (sort-by :t)))

(defn shade-hit [w comps]
  (color 1 1 1))
