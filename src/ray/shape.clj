(ns ray.shape
  (:require
    [ray.matrix :refer [id inverse subtract]]
    [ray.ray :refer [direction origin transform]]
    [ray.math :refer [sqrt square]]
    [ray.tuple :refer [dot point]]))

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
        sphere->ray (subtract o (point 0 0 0))
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
  (->> is
       (drop-while #(neg? (:t %)))
       first))
