(ns ray.lights-test
  (:require
   pjstadig.humane-test-output
   [clojure.test :refer [deftest is testing]]
   [ray.color :refer [color]]
   [ray.light :refer [->PointLight]]
   [ray.point3 :refer [point3]]))

(pjstadig.humane-test-output/activate!)

(deftest lights.feature
  (testing "A point light has a position and intensity"
    (let [intensity (color 1 1 1)
          position  (point3 0 0 0)
          light     (->PointLight position intensity)]
      (is (= (:position light) position))
      (is (= (:intensity light) intensity)))))
