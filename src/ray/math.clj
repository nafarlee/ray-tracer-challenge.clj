(ns ray.math)

(def EPSILON 0.00001)

(defn abs [^double x]
  (Math/abs x))

(defn float= [^double a ^double b]
  (> EPSILON (abs (- a b))))

(defn pow [x e]
  (Math/pow x e))

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

(defn sqrt [x]
  (Math/sqrt x))

(defn round [^double x]
  (Math/round x))
