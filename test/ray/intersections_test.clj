(ns ray.intersections-test
  (:require
   [clojure.test :refer [deftest is testing]]
   pjstadig.humane-test-output
   [ray.point3 :refer [point3]]
   [ray.vector3 :refer [vector3]]
   [ray.shape :refer [sphere hit intersect intersection intersections]]
   [ray.ray :refer [ray]]))

(pjstadig.humane-test-output/activate!)

(deftest intersections.feature
  (testing "An intersection encapsulates t and object"
    (let [s (sphere)
          i (intersection 3.5 s)]
      (is (= (:t i) 3.5))
      (is (= (:object i) s))))

  (testing "Aggregating intersections"
    (let [s (sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i1 i2)]
      (is (= (mapv :t xs) [1 2]))))

  (testing "Intersect sets the object on the intersection"
    (let [r (ray (point3 0 0 -5) (vector3 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= (mapv :object xs) [s s]))))

  (testing "The hit, when all intersections have positive t"
    (let [s (sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i1 i2)]
      (is (= (hit xs) i1))))

  (testing "The hit, when some intersections have negative t"
    (let [s (sphere)
          i1 (intersection -1 s)
          i2 (intersection 1 s)
          xs (intersections i1 i2)]
      (is (= (hit xs) i2))))

  (testing "The hit, when all intersections have negative t"
    (let [s (sphere)
          i1 (intersection -2 s)
          i2 (intersection -1 s)
          xs (intersections i1 i2)]
      (is (= (hit xs) nil))))

  (testing "The hit is always the lowest nonnegative intersection"
    (let [s (sphere)
          i1 (intersection 5 s)
          i2 (intersection 7 s)
          i3 (intersection -3 s)
          i4 (intersection 2 s)
          xs (intersections i1 i2 i3 i4)]
      (is (= (hit xs) i4)))))
