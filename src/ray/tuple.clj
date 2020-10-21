(ns ray.tuple
  (:require
    [clojure.spec.alpha :as s]
    [ray.matrix :as m]
    [ray.math :refer [float= square]]))

(defn map-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(def zip (partial map vector))

(s/def ::tuple (s/tuple (s/tuple float?)
                        (s/tuple float?)
                        (s/tuple float?)
                        (s/tuple #{1.0 0.0})))
(defn tuple [x y z w]
  [[x]
   [y]
   [z]
   [w]])

(def tuple? (partial s/valid? ::tuple))

(defn multiply [t x]
  (m/fmap (fn [e _ _] (* e x)) t))

(defn divide [t x]
  (m/fmap (fn [e _ _] (/ e x)) t))

(defn point [x y z]
  (tuple x y z 1.0))

(defn point?  [[_ _ _ [w] :as t]]
  (and (tuple? t)
       (== 1.0 w)))

(defn vector' [x y z]
  (tuple x y z 0.0))

(defn vector'?  [[_ _ _ [w] :as t]]
  (and (tuple? t)
       (zero? w)))

(defn pointwise [f a b]
  (mapv #(mapv f %1 %2) a b))

(def add (partial pointwise +))

(def subtract (partial merge-with -))

(def negate (partial subtract (vector' 0 0 0)))

(defn magnitude [v]
  (->> (vals v)
       (map square)
       (apply +)
       Math/sqrt))

(defn normalize [v]
  (let [m (magnitude v)]
    (->> ((juxt ::x ::y ::z) v)
         (map #(/ % m))
         (apply vector'))))

(defn dot [a b]
  (->> (merge-with * a b)
       vals
       (apply +)))

(defn cross [{ax ::x ay ::y az ::z} {bx ::x by ::y bz ::z}]
  (vector' (- (* ay bz)
              (* az by))
           (- (* az bx)
              (* ax bz))
           (- (* ax by)
              (* ay bx))))

(def hadamard (partial merge-with *))

(defn eq [a b]
  (every? (partial apply float=)
          (zip (vals a) (vals b))))
