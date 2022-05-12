(ns ray.rays-test
  (:require
   [clojure.test :refer [deftest is testing]]
   pjstadig.humane-test-output
   [ray.matrix :refer [scaling translation]]
   [ray.point3 :refer [point3]]
   [ray.vector3 :refer [vector3]]
   [ray.ray :refer [ray origin direction position transform]]))

(pjstadig.humane-test-output/activate!)

(deftest rays.feature
  (testing "Creating and querying a ray"
    (let [o (point3 1 2 3)
          d (vector3 4 5 6)
          r (ray o d)]
      (is (= (origin r) o))
      (is (= (direction r) d))))

  (testing "Computing a point from a distance"
    (let [r (ray (point3 2 3 4) (vector3 1 0 0))]
      (is (= (position r 0) (point3 2 3 4)))
      (is (= (position r 1) (point3 3 3 4)))
      (is (= (position r -1) (point3 1 3 4)))
      (is (= (position r 2.5) (point3 4.5 3 4)))))

  (testing "Translating a ray"
    (let [r (ray (point3 1 2 3) (vector3 0 1 0))
          m (translation 3 4 5)
          r2 (transform r m)]
      (is (= (origin r2) (point3 4 6 8)))
      (is (= (direction r2) (vector3 0 1 0)))))

  (testing "Scaling a ray"
    (let [r (ray (point3 1 2 3) (vector3 0 1 0))
          m (scaling 2 3 4)
          r2 (transform r m)]
      (is (= (origin r2) (point3 2 6 12)))
      (is (= (direction r2) (vector3 0 3 0))))))
