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

(defn- ambient [m l]
  {:pre [(is (Material? m))
         (is (PointLight? l))]}
  (scalar-multiply
   (hadamard (:color m) (:intensity l))
   (:ambient m)))

(defn- diffuse [m l p n]
  {:pre [(is (Material? m))
         (is (PointLight? l))
         (is (point3? p))
         (is (vector3? n))]}
  (let [light-vector (normalize (subtract (:position l) p))
        lightVnormal (dot light-vector n)]
    (if (neg? lightVnormal)
      black
      (scalar-multiply
       (hadamard (:color m) (:intensity l))
       (* (:diffuse m)
          lightVnormal)))))

(defn lighting [m l p e n]
  {:pre [(is (Material? m))
         (is (PointLight? l))
         (is (point3? p))
         (is (vector3? e))
         (is (vector3? n))]}
  (let [effective-color     (hadamard (:color m) (:intensity l))
        light-vector        (normalize (subtract (:position l) p))
        ambient             (scalar-multiply effective-color (:ambient m))
        light-dot-normal    (dot light-vector n)]
    (if (< light-dot-normal 0)
      ambient
      (let [diffuse         (scalar-multiply effective-color
                                             (* (:diffuse m)
                                                light-dot-normal))
            reflect-vector  (reflect (negate light-vector) n)
            reflect-dot-eye (dot reflect-vector e)
            specular        (if-not (pos? reflect-dot-eye)
                              black
                              (scalar-multiply (:intensity l)
                                               (* (:specular m)
                                                  (pow reflect-dot-eye
                                                       (:shininess m)))))]
       (add ambient diffuse specular)))))
