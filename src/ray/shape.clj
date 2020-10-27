(ns ray.shape
  (:require
    [ray.math :refer [sqrt square]]
    [ray.tuple :refer [dot subtract point]]))

(defn sphere []
  {})

(defn intersection [t s]
  {:t t
   :object s})

(defn intersect [s {:keys [direction origin]}]
  (let [sphere->ray (subtract origin (point 0 0 0))
        a (dot direction direction)
        b (* 2 (dot direction sphere->ray))
        c (dec (dot sphere->ray sphere->ray))
        discriminant (- (square b) (* 4 a c))]
    (if (neg? discriminant)
      []
      [(intersection (/ (- (- b) (sqrt discriminant)) (* 2 a)) s)
       (intersection (/ (+ (- b) (sqrt discriminant)) (* 2 a)) s)])))

(defn intersections [& is]
  (apply sorted-set-by #(compare (:t %1) (:t %2)) is))

(defn hit [is]
  (->> is
       (drop-while #(neg? (:t %)))
       first))
