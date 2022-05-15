(ns ray.camera
  (:require
   [ray.math :refer [tan]]
   [ray.matrix :refer [id]]))

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
