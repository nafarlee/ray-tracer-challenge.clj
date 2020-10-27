(ns ray.shape
  (:require
    [ray.math :refer [sqrt square]]
    [ray.tuple :refer [dot subtract point]]))

(defn sphere []
  {})

(defn intersect [s {:keys [direction origin]}]
  (let [sphere->ray (subtract origin (point 0 0 0))
        a (dot direction direction)
        b (* 2 (dot direction sphere->ray))
        c (dec (dot sphere->ray sphere->ray))
        discriminant (- (square b) (* 4 a c))]
    (if (neg? discriminant)
      []
      [(/ (- (- b) (sqrt discriminant)) (* 2 a))
       (/ (+ (- b) (sqrt discriminant)) (* 2 a))])))
