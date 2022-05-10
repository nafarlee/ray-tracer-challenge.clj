(ns ray.shape
  (:require
    [clojure.test :refer [is]]
    [ray.material :refer [material]]
    [ray.matrix :refer [id
                        inverse
                        multiply
                        negate
                        submatrix
                        subtract
                        transpose]]
    [ray.ray :refer [direction origin position transform]]
    [ray.math :refer [sqrt square]]
    [ray.point3 :refer [point3]]
    [ray.vector3 :refer [vector3?]]
    [ray.tuple :refer [dot normalize]]))

(defrecord Sphere [transform material])

(defrecord Computations [t object point eyev normalv inside])

(defn sphere
  ([] (sphere {}))
  ([& {:as opts}]
   (map->Sphere
    (merge
     {:transform id
      :material (material)}
     opts))))

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
  {:post [(is (vector3? %))]}
  (let [object-point  (multiply (inverse (:transform sph)) world-point)
        object-normal (subtract object-point (point3 0 0 0))
        world-normal  (multiply (-> (:transform sph)
                                    (submatrix 3 3)
                                    inverse
                                    transpose)
                                (submatrix object-normal 3 3))]
    (normalize (conj world-normal [0]))))

(defn prepare-computations [intersection ray]
  (let [t       (:t intersection)
        object  (:object intersection)
        point   (position ray t)
        eyev    (negate (direction ray))
        normalv (normal-at object point)
        inside  (neg? (dot normalv eyev))
        normalv (if inside
                  (negate normalv)
                  normalv)]
    (->Computations t object point eyev normalv inside)))
