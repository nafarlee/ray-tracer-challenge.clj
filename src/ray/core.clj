(ns ray.core
  (:require
    [ray.camera :refer [camera render]]
    [ray.transform :refer [view-transform]]
    [ray.world :refer [->World]]
    [ray.material :refer [material]]
    [ray.ray :refer [direction position ray]]
    [ray.shape :refer [intersect hit normal-at sphere]]
    [ray.matrix :refer [add
                        chain
                        negate
                        rotation-x
                        rotation-y
                        rotation-z
                        scalar-multiply
                        scaling
                        subtract
                        translation]]
    [ray.math :refer [pi]]
    [ray.point3 :refer [point3]]
    [ray.vector3 :refer [vector3]]
    [ray.tuple :refer [normalize]]
    [ray.canvas :refer [canvas write-pixel]]
    [ray.color :refer [color]]
    [ray.light :refer [->PointLight lighting]]
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
        shape       (assoc-in (sphere) [:material :color] (color 1 0.2 1))
        light       (->PointLight (point3 -10 10 -10) (color 1 1 1))
        ray-origin  (point3 0 0 -5)
        wall-z      10
        render      (fn [c [x y]]
                      (let [world-x       (- (* pixel-size x) half)
                            world-y       (- half (* pixel-size y))
                            brick         (point3 world-x world-y wall-z)
                            origin=>brick (subtract brick ray-origin)
                            r             (ray ray-origin
                                               (normalize origin=>brick))
                            h             (hit (intersect shape r))
                            pos           (delay (position r (:t h)))
                            normal        (delay (normal-at (:object h) @pos))
                            eye           (delay (negate (direction r)))]
                        (if-not h
                          c
                          (write-pixel c
                                       (int x)
                                       (int y)
                                       (lighting (get-in h [:object :material])
                                                 light
                                                 @pos
                                                 @eye
                                                 @normal)))))]
    (->> (for [x (range canvas-size)
               y (range canvas-size)]
           [x y])
         (reduce render canvas)
         canvas->ppm
         (spit "output.ppm"))))

(defn chapter-7 []
  (let [wall-material (material :color (color 1 0.9 0.9) :specular 0)
        floor (sphere :transform (scaling 10 0.01 10) :material wall-material)
        left-wall (sphere
                   :transform
                   (chain
                    (translation 0 0 5)
                    (rotation-y (- (/ pi 4)))
                    (rotation-x (/ pi 2))
                    (scaling 10 0.01 10))
                   :material
                   wall-material)
        right-wall (sphere
                    :transform
                    (chain
                     (translation 0 0 5)
                     (rotation-y (/ pi 4))
                     (rotation-x (/ pi 2))
                     (scaling 10 0.01 10))
                    :material
                    wall-material)
        middle (sphere
                :transform (translation -0.5 1 0.5)
                :material
                (material
                 :color (color 0.1 1 0.5)
                 :diffuse 0.7
                 :specular 0.3))
        world (->World
               [floor left-wall right-wall middle]
               (->PointLight
                (point3 -10 10 -10)
                (color 1 1 1)))
        camera (camera
                100
                50
                (/ pi 3)
                (view-transform
                 (point3 0 1.5 -5)
                 (point3 0 1 0)
                 (vector3 0 1 0)))]
    (spit "output.ppm" (canvas->ppm (render camera world)))))

(def -main chapter-7)
