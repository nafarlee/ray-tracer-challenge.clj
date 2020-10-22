(ns ray.math)

(def EPSILON 0.00001)

(defn float= [a b]
  (->> a
       (- b)
       Math/abs
       (> EPSILON)))

(defn square [x]
  (Math/pow x 2))

(defn clamp [low high x]
  (-> x
      (max low)
      (min high)))

(def pi Math/PI)

(defn sin [x]
  (Math/sin x))

(defn cos [x]
  (Math/cos x))
