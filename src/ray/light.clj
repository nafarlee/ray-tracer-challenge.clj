(ns ray.light
  (:require
   [clojure.test :refer [is]]
   [ray.color :refer [black]]
   [ray.tuple :refer [dot normalize]]
   [ray.point3 :refer [point3?]]
   [ray.vector3 :refer [reflect vector3?]]
   [ray.material :refer [Material?]]
   [ray.math :refer [pow]]
   [ray.matrix :refer [add hadamard negate scalar-multiply subtract]]))

(defrecord PointLight [position intensity])

(def PointLight? (partial instance? PointLight))

(defn- ambient
  [{:keys [color ambient]}
   {:keys [intensity]}]
  (scalar-multiply (hadamard color intensity) ambient))

(defn- diffuse
  [{:keys [color diffuse]}
   {:keys [position intensity]}
   point
   normal]
  (let [light:normal (dot (normalize (subtract position point)) normal)]
    (if (neg? light:normal)
      black
      (scalar-multiply (hadamard color intensity)
                       (* diffuse light:normal)))))

(defn- specular
  [{:keys [specular shininess]}
   {:keys [position intensity]}
   point
   eye
   normal]
  (let [point=>light   (normalize (subtract position point))
        reflection:eye (delay (dot (reflect (negate point=>light) normal) eye))]
    (cond
      (neg? (dot point=>light normal)) black
      (not (pos? @reflection:eye))     black
      :else                            (scalar-multiply
                                        intensity
                                        (* specular
                                           (pow @reflection:eye shininess))))))

(defn lighting [m l p e n]
  {:pre [(is (Material? m))
         (is (PointLight? l))
         (is (point3? p))
         (is (vector3? e))
         (is (vector3? n))]}
  (add
   (ambient m l)
   (diffuse m l p n)
   (specular m l p e n)))
