(ns ray.core
  (:require
    [ray.ray :refer [ray]]
    [ray.shape :refer [intersect hit sphere]]
    [ray.matrix :as m]
    [ray.math :refer [pi]]
    [ray.tuple :as rt]
    [ray.canvas :as rcan]
    [ray.color :as rc]
    [ray.ppm :as rp])
  (:gen-class))

(defn chapter-2 []
  (let [position (rt/point 0 1 0)
        velocity (-> (rt/vector' 1 1.8 0) rt/normalize (rt/multiply 11.25))
        gravity (rt/vector' 0 -0.1 0)
        wind (rt/vector' -0.01 0 0)
        c (rcan/canvas 900 550)
        white (rc/color 255 255 255)]
    (loop [p position, v velocity, c c]
      (if (every? #(<= 0 %) [(::rt/x p) (::rt/y p)])
        (recur
          (rt/add p v)
          (reduce rt/add [v gravity wind])
          (rcan/write-pixel c
                            (->> p ::rt/x int)
                            (->> p ::rt/y (- (::rcan/height c)) int)
                            white))
        (spit "output.ppm" (rp/canvas->ppm c))))))

(defn chapter-4 []
  (let [size 100
        point (rt/point 0 0 0)
        slide-out (m/translation (* 0.49 size) 0 0)
        center (m/translation (/ size 2) (/ size 2) 0)
        canvas (rcan/canvas size size)
        white (rc/color 255 255 255)]
    (->> (range 0 (* 2 pi) (/ pi 6))
         (mapv #(m/chain point slide-out (m/rotation-z %) center))
         (reduce (fn [c [[x] [y]]] (rcan/write-pixel c (int x) (int y) white))
                 canvas)
         rp/canvas->ppm
         (spit "output.ppm"))))

(defn chapter-6 []
  (let [wall-size 7
        half (/ wall-size 2)
        canvas-size 100
        canvas (rcan/canvas canvas-size canvas-size)
        pixel-size (/ wall-size canvas-size)
        shape (sphere)
        red (rc/color 1 0 0)
        ray-origin (rt/point 0 0 -5)
        wall-z 10]
    (->> (for [x (range canvas-size)
               y (range canvas-size)]
           [x y])
         (filter (fn [[x y]]
                   (let [world-x (- (* pixel-size x) half)
                         world-y (- half (* pixel-size y))]
                     (as-> (rt/point world-x world-y wall-z) $
                           (rt/subtract $ ray-origin)
                           (rt/normalize $)
                           (ray ray-origin $)
                           (intersect shape $)
                           (hit $)))))
         (reduce (fn [c [x y]]
                   (rcan/write-pixel c (int x) (int y) red))
                 canvas)
         rp/canvas->ppm
         (spit "output.ppm"))))

(def -main chapter-6)
