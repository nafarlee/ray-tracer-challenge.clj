(ns ray.camera
  (:require
   (ray.matrix :refer [id])))

(defrecord Camera [hsize vsize field-of-view transform])

(defn camera
  ([hsize vsize field-of-view transform]
   (->Camera hsize vsize field-of-view transform))
  ([hsize vsize field-of-view]
   (camera hsize vsize field-of-view id)))
