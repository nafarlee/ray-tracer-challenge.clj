(ns ray.matrix
  (:require
    [ray.math :refer [cos float= sin]]))

(defn at [m x y]
  (-> m
      (nth x)
      (nth y)))

(defn row [m r]
  (nth m r))

(defn column [m c]
  (map #(nth % c) m))

(def rows identity)

(defn columns [m]
  (apply mapv vector m))

(defn dot [r c]
  (apply + (mapv * r c)))

(defn multiply [a b]
  (->> (for [r (rows a)
             c (columns b)]
         (dot r c))
       (partition (count (first b)))
       (mapv vec)))

(defn chain [& ms]
  (reduce multiply (reverse ms)))

(def transpose columns)

(def id [[1 0 0 0]
         [0 1 0 0]
         [0 0 1 0]
         [0 0 0 1]])

(defn size [m]
  [(count m) (count (first m))])

(defn determinant2 [[[a b]
                     [c d]]]
  (- (* a d) (* b c)))

(defn submatrix [m r c]
  (->> m
       (keep-indexed #(if (== r %1) nil %2))
       (mapv (partial keep-indexed #(if (== c %1) nil %2)))))

(declare cofactor)
(defn determinant [m]
  (if (= [2 2] (size m))
    (determinant2 m)
    (apply + (for [r [0]
                   c (->> m first count range)]
               (* (at m r c) (cofactor m r c))))))

(defn minor [m r c]
  (-> m
      (submatrix r c)
      determinant))

(defn cofactor [m r c]
  (as-> m $
        (minor $ r c)
        (if (odd? (+ r c))
          (- $)
          $)))

(defn invertible? [m]
  (->> m
       determinant
       zero?
       not))

(defn fmap [f m]
  (->> (for [r (->> m size first range)
             c (->> m size second range)]
         (f (at m r c) r c))
       (partition (count (first m)))
       (mapv vec)))

(defn scalar-multiply [t x]
  (fmap (fn [e _ _] (* e x)) t))

(defn scalar-divide [t x]
  (fmap (fn [e _ _] (/ e x)) t))

(defn inverse [m]
  (let [det (determinant m)]
    (when (not (zero? det))
      (fmap (fn [_ r c] (/ (cofactor m c r) det)) m))))

(defn eq [a b]
  (->> (mapv float= (flatten a) (flatten b))
       (every? identity)))

(defn translation [x y z]
  [[1 0 0 x]
   [0 1 0 y]
   [0 0 1 z]
   [0 0 0 1]])

(defn scaling [x y z]
  [[x 0 0 0]
   [0 y 0 0]
   [0 0 z 0]
   [0 0 0 1]])

(defn rotation-x [r]
  [[1 0       0           0]
   [0 (cos r) (- (sin r)) 0]
   [0 (sin r) (cos r)     0]
   [0 0       0           1]])

(defn rotation-y [r]
  [[(cos r)     0 (sin r) 0]
   [0           1 0       0]
   [(- (sin r)) 0 (cos r) 0]
   [0           0 0       1]])

(defn rotation-z [r]
  [[(cos r) (- (sin r)) 0 0]
   [(sin r) (cos r)     0 0]
   [0       0           1 0]
   [0       0           0 1]])

(defn shearing [xy xz yx yz zx zy]
  [[1  xy xz 0]
   [yx 1  yz 0]
   [zx zy 1  0]
   [0  0  0  1]])

(defn entrywise [f a b]
  (mapv (partial mapv f) a b))

(def add (partial entrywise +))

(def subtract (partial entrywise -))

(defn negate [t]
  (fmap (fn [e _ _] (- e)) t))

(def hadamard (partial entrywise *))
