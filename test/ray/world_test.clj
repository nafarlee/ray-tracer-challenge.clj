(ns ray.world-test
  (:require
   pjstadig.humane-test-output
   [clojure.test :refer [deftest is testing]]
   [ray.matrix :refer [eq scaling]]
   [ray.material :refer [material]]
   [ray.point3 :refer [point3]]
   [ray.vector3 :refer [vector3]]
   [ray.color :refer [color]]
   [ray.shape :refer [sphere intersection prepare-computations]]
   [ray.light :refer [->PointLight]]
   [ray.ray :refer [ray]]
   [ray.world :refer [color-at world default-world intersect-world shade-hit]]))

(pjstadig.humane-test-output/activate!)

(deftest world.feature
  (testing "Creating a world"
    (let [w (world)]
      (is (empty? (:objects w)))
      (is (nil? (:light w)))))

  (testing "The default world"
    (let [light (->PointLight (point3 -10 -10 -10) (color 1 1 1))
          s1    (sphere
                 :material
                 (material
                  :color    (color 0.8 1.0 0.6)
                  :diffuse  0.7
                  :specular 0.2))
          s2    (sphere :transform (scaling 0.5 0.5 0.5))
          w (default-world)]
      (is (= (:light w) light))
      (is (some (partial = s1) (:objects w)))
      (is (some (partial = s2) (:objects w)))))

  (testing "Intersect a world with a ray"
    (let [w  (default-world)
          r  (ray (point3 0 0 -5) (vector3 0 0 1))
          xs (intersect-world w r)]
      (is (== 4 (count xs) 4))
      (is (== 4 (:t (nth xs 0))))
      (is (== 4.5 (:t (nth xs 1))))
      (is (== 5.5 (:t (nth xs 2))))
      (is (== 6 (:t (nth xs 3))))))

  (testing "Shading an intersection"
    (let [w     (default-world)
          r     (ray (point3 0 0 -5) (vector3 0 0 1))
          shape (first (:objects w))
          i     (intersection 4 shape)
          comps (prepare-computations i r)
          c     (shade-hit w comps)]
      (is (eq (color 0.38066 0.47583 0.2855) c))))

  (testing "The color when a ray misses"
    (let [w (default-world)
          r (ray (point3 0 0 -5) (vector3 0 1 0))
          c (color-at w r)]
      (is (eq (color 0 0 0) c))))

  (testing "The color when a ray hits"
    (let [w (default-world)
          r (ray (point3 0 0 -5) (vector3 0 0 1))
          c (color-at w r)]
      (is (eq (color 0.38066 0.47583 0.2855) c)))))
