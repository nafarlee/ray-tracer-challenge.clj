(ns ray.spheres-test
  (:require
   [clojure.test :refer [deftest is testing]]
   pjstadig.humane-test-output
   [ray.ray :refer [ray]]
   [ray.point3 :refer [point3]]
   [ray.vector3 :refer [vector3]]
   [ray.shape :refer [intersect sphere]]))

(pjstadig.humane-test-output/activate!)

(deftest spheres.feature
  (testing "A ray intersects a sphere at two points"
    (let [r (ray (point3 0 0 -5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :t xs) [4.0 6.0]))))

  (testing "A ray intersects a sphere at a tangent"
    (let [r (ray (point3 0 1 -5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :t xs) [5.0 5.0]))))

  (testing "A ray misses a sphere"
    (let [r (ray (point3 0 2 -5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= xs []))))

  (testing "A ray originates inside a sphere"
    (let [r (ray (point3 0 0 0) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :t xs) [-1.0 1.0]))))

  (testing "A sphere is behind a ray"
    (let [r (ray (point3 0 0 5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :t xs) [-6.0 -4.0])))))
