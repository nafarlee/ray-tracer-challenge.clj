(ns ray.camera
  (:require
   [clojure.test :refer [is]]
   [ray.canvas :refer [canvas? ->Canvas]]
   [ray.world :refer [color-at]]
   [ray.tuple :refer [normalize]]
   [ray.ray :refer [ray]]
   [ray.point3 :refer [point3]]
   [ray.math :refer [tan]]
   [ray.matrix :refer [id inverse multiply subtract]]))

(defrecord Camera [hsize vsize field-of-view transform])

(defn camera
  ([hsize vsize field-of-view transform]
   (->Camera hsize vsize field-of-view transform))
  ([hsize vsize field-of-view]
   (camera hsize vsize field-of-view id)))

(def half-width
  (memoize
   (fn [{:keys [field-of-view hsize vsize]}]
     (let [half-view (tan (/ field-of-view 2))
           aspect    (/ hsize vsize)]
       (if (>= aspect 1)
         half-view
         (* half-view aspect))))))

(def half-height
  (memoize
   (fn [{:keys [field-of-view hsize vsize]}]
     (let [half-view (tan (/ field-of-view 2))
           aspect    (/ hsize vsize)]
       (if (>= aspect 1)
         (/ half-view aspect)
         half-view)))))

(def pixel-size
  (memoize
   (fn [{:keys [hsize] :as camera}]
     (/ (* 2 (half-width camera)) hsize))))

(defn ray-for-pixel [{:keys [transform] :as camera} px py]
  (let [xoffset   (* (pixel-size camera) (+ px 0.5))
        yoffset   (* (pixel-size camera) (+ py 0.5))
        world-x   (- (half-width camera) xoffset)
        world-y   (- (half-height camera) yoffset)
        pixel     (multiply (inverse transform)
                            (point3 world-x world-y -1))
        origin    (multiply (inverse transform)
                            (point3 0 0 0))
        direction (normalize (subtract pixel origin))]
    (ray origin direction)))

(defn render [{:keys [hsize vsize] :as camera} world]
  {:post [(is (canvas? %))]}
  (-> (for [x (range hsize)
            y (range vsize)]
        (color-at world (ray-for-pixel camera x y)))
      vec
      (->Canvas hsize vsize)))
