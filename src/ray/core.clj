(ns ray.core
  (:require
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

(def -main chapter-2)
