(ns ray.shape
  (:require
    [ray.matrix :refer [id inverse multiply submatrix subtract transpose]]
    [ray.ray :refer [direction origin transform]]
    [ray.math :refer [sqrt square]]
    [ray.point3 :refer [point3]]
    [ray.tuple :refer [dot normalize]]))

(defrecord Sphere [transform])

(defn sphere
  ([] (->Sphere id))
  ([m] (->Sphere m)))

(defn intersection [t s]
  {:t t
   :object s})

(defn intersect [s r]
  (let [r2 (transform r (inverse (:transform s)))
        o (origin r2)
        d (direction r2)
        sphere->ray (subtract o (point3 0 0 0))
        a (dot d d)
        b (* 2 (dot d sphere->ray))
        c (dec (dot sphere->ray sphere->ray))
        discriminant (- (square b) (* 4 a c))]
    (if (neg? discriminant)
      []
      [(intersection (/ (- (- b) (sqrt discriminant)) (* 2 a)) s)
       (intersection (/ (+ (- b) (sqrt discriminant)) (* 2 a)) s)])))

(defn intersections [& is]
  (apply sorted-set-by #(compare (:t %1) (:t %2)) is))

(defn hit [is]
  (as-> is <>
        (drop-while #(neg? (:t %)) <>)
        (nth <> 0 nil)))

(defn normal-at [sph world-point]
  (let [object-point  (multiply (inverse (:transform sph))
                                world-point)
        object-normal (subtract object-point (point3 0 0 0))
        world-normal  (multiply (transpose (inverse (:transform sph)))
                                object-normal)]
    (normalize (assoc world-normal 3 [0]))))
