(ns ray.world
  (:require
   [ray.point3 :refer [point3]]
   [ray.light :refer [->PointLight lighting]]
   [ray.matrix :refer [scaling]]
   [ray.color :refer [black color]]
   [ray.material :refer [material]]
   [ray.shape :refer [sphere intersect prepare-computations]]))

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

(defn shade-hit [{:keys [light]} {:keys [object point eyev normalv]}]
  (lighting (:material object) light point eyev normalv))

(defn color-at [w r]
  (let [intersections (intersect-world w r)]
    (if (empty? intersections)
      black
      (shade-hit w (prepare-computations (first intersections) r)))))
