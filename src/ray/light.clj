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

(defn- specular [m l p e n]
  (let [point=>light   (normalize (subtract (:position l) p))
        reflection:eye (delay (dot (reflect (negate point=>light) n) e))]
    (cond
      (neg? (dot point=>light n))  black
      (not (pos? @reflection:eye)) black
      :else                        (scalar-multiply
                                    (:intensity l)
                                    (* (:specular m)
                                       (pow @reflection:eye
                                            (:shininess m)))))))

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
