(ns ray.core
  (:require
   [clojure.spec.alpha :as s]
   [clojure.pprint :refer [pprint]])
  (:gen-class))

(defprotocol Multiply (multiply [this scalar]))

(defprotocol Divide (divide [this scalar]))

(def EPSILON 0.00001)

(defn float=
  [a b]
  (->> a
       (- b)
       Math/abs
       (> EPSILON)))

(defn map-values
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(def zip (partial map vector))

(defn eq
  [a b]
  (every? (partial apply float=)
          (zip (vals a) (vals b))))

(s/def ::x float?)
(s/def ::y float?)
(s/def ::z float?)
(s/def ::w #{1.0 0.0})
(s/def ::tuple (s/keys :req [::x ::y ::z ::w]))
(defn tuple
  [x y z w]
  {::x x, ::y y, ::z z, ::w w})

(def tuple? (partial s/valid? ::tuple))

(defn multiply
  [t x]
  (map-values (partial * x) t))

(defn divide
  [t x]
  (map-values #(/ % x) t))

(defn point [x y z]
  (tuple x y z 1))

(defn point? [{w ::w :as t}]
  (and (tuple? t)
       (== 1.0 w)))

(defn vector' [x y z]
  (tuple x y z 0))

(defn vector'? [{w ::w :as t}]
  (and (tuple? t)
       (zero? w)))

(s/def ::red float?)
(s/def ::green float?)
(s/def ::blue float?)
(s/def ::color (s/keys :req [::red ::green ::blue]))
(defn color
  [red green blue]
  {::red red, ::green green, ::blue blue})

(def add (partial merge-with +))

(def subtract (partial merge-with -))

(def negate (partial subtract (vector' 0 0 0)))

(defn magnitude [v]
  (Math/sqrt (+ (Math/pow (::x v) 2)
                (Math/pow (::y v) 2)
                (Math/pow (::z v) 2))))

(defn normalize
  [v]
  (let [m (magnitude v)]
    (->> ((juxt ::x ::y ::z) v)
         (map #(/ % m))
         (apply vector'))))

(defn dot [a b]
  (->> (merge-with * a b)
       vals
       (apply +)))

(defn cross
  [{ax ::x ay ::y az ::z} {bx ::x by ::y bz ::z}]
  (vector' (- (* ay bz)
              (* az by))
           (- (* az bx)
              (* ax bz))
           (- (* ax by)
              (* ay bx))))

(def hadamard (partial merge-with *))

(defrecord Canvas [pixels width height])

(defn canvas
  [w h]
  (-> (* w h)
      (repeat (color 0 0 0))
      vec
      (->Canvas w h)))

(defn write-pixel
  [{w :width h :height ps :pixels} x y color]
    (-> ps
        (assoc (+ x (* y w)) color)
        (->Canvas w h)))

(defn pixel-at
  [{w :width ps :pixels} x y]
  (nth ps (+ x (* y w))))

(defrecord Projectile [position velocity])

(defrecord Environment [gravity wind])

(defn tick
  [{g :gravity w :wind} {p :position v :velocity}]
  (let [position (add p v)
        velocity (reduce add [v g w])]
    (->Projectile position velocity)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [gravity (vector' 0 -0.1 0)
        wind (vector' -0.01 0 0)
        environment (->Environment gravity wind)
        position (point 0 1 0)
        velocity (vector' 1 1 0)
        initial (->Projectile position velocity)
        f (partial tick environment)]
    (->> initial
         (iterate f)
         (take-while (comp (partial <= 0) ::y :position))
         pprint)))
