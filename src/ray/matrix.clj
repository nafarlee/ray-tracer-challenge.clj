(ns ray.matrix)

(defn at [m x y]
  (-> m
      (nth x)
      (nth y)))
