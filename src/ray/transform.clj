(ns ray.transform
  (:require
    [ray.matrix :refer [multiply translation subtract]]
    [ray.tuple :refer [normalize]]
    [ray.vector3 :refer [cross vector3?]]
    [ray.point3 :refer [point3?]]))

(defn view-transform [[[from-x] [from-y] [from-z] :as from] to up]
  {:pre [(point3? from)
         (point3? to)
         (vector3? up)]}
  (let [[[forward:x]
         [forward:y]
         [forward:z]
         :as forward] (normalize (subtract to from))
        [[left:x]
         [left:y]
         [left:z]
         :as left]    (cross forward (normalize up))
        [[true-up:x]
         [true-up:y]
         [true-up:z]] (cross left forward)]
    (multiply
     [[left:x        left:y        left:z        0]
      [true-up:x     true-up:y     true-up:z     0]
      [(- forward:x) (- forward:y) (- forward:z) 0]
      [0             0             0             1]]
     (translation (- from-x) (- from-y) (- from-z)))))
