(ns ray.core
  (:require
    [ray.ray :refer [ray]]
    [ray.shape :refer [intersect hit sphere]]
    [ray.matrix :refer [add chain rotation-z scalar-multiply subtract translation]]
    [ray.math :refer [pi]]
    [ray.point3 :refer [point3]]
    [ray.vector3 :refer [vector3]]
    [ray.tuple :refer [normalize]]
    [ray.canvas :refer [canvas write-pixel]]
    [ray.color :refer [color]]
    [ray.ppm :refer [canvas->ppm]])
  (:gen-class))

(defn chapter-2 []
  (let [position (point3 0 1 0)
        velocity (-> (vector3 1 1.8 0) normalize (scalar-multiply 11.25))
        gravity (vector3 0 -0.1 0)
        wind (vector3 -0.01 0 0)
        c (canvas 900 550)
        white (color 255 255 255)]
    (loop [p position, v velocity, c c]
      (if (every? #(<= 0 %) [(::x p) (::y p)])
        (recur
          (add p v)
          (reduce add [v gravity wind])
          (write-pixel c
                            (->> p ::x int)
                            (->> p ::y (- (::height c)) int)
                            white))
        (spit "output.ppm" (canvas->ppm c))))))

(defn chapter-4 []
  (let [size 100
        point (point3 0 0 0)
        slide-out (translation (* 0.49 size) 0 0)
        center (translation (/ size 2) (/ size 2) 0)
        canvas (canvas size size)
        white (color 255 255 255)]
    (->> (range 0 (* 2 pi) (/ pi 6))
         (mapv #(chain point slide-out (rotation-z %) center))
         (reduce (fn [c [[x] [y]]] (write-pixel c (int x) (int y) white))
                 canvas)
         canvas->ppm
         (spit "output.ppm"))))

(defn chapter-5 []
  (let [wall-size   7
        half        (/ wall-size 2)
        canvas-size 100
        canvas      (canvas canvas-size canvas-size)
        pixel-size  (/ wall-size canvas-size)
        shape       (sphere)
        ray-origin  (point3 0 0 -5)
        wall-z      10
        hit?        (fn [[x y]]
                      (let [world-x (- (* pixel-size x) half)
                            world-y (- half (* pixel-size y))]
                        (as-> (point3 world-x world-y wall-z) $
                              (subtract $ ray-origin)
                              (normalize $)
                              (ray ray-origin $)
                              (intersect shape $)
                              (hit $))))
        draw-hit    (fn [c [x y]]
                      (write-pixel c
                                   (int x)
                                   (int y)
                                   (color (/ x canvas-size)
                                          (/ y canvas-size)
                                          1)))]
    (->> (for [x (range canvas-size)
               y (range canvas-size)]
           [x y])
         (filter hit?)
         (reduce draw-hit canvas)
         canvas->ppm
         (spit "output.ppm"))))

(def -main chapter-5)
