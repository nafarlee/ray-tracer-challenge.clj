(ns ray.transformations-test
  (:require
   [clojure.test :refer [deftest is testing]]
   pjstadig.humane-test-output
   [ray.point3 :refer [point3]]
   [ray.vector3 :refer [vector3]]
   [ray.math :refer [pi sqrt]]
   [ray.matrix
    :refer
    [translation
     inverse
     scaling
     rotation-x
     rotation-y
     rotation-z
     shearing
     eq
     multiply]]))
     
(pjstadig.humane-test-output/activate!)

(deftest transformations.feature
  (testing "Multiplying by a translation matrix"
    (let [transform (translation 5 -3 2)
          p (point3 -3 4 5)]
      (is (= (multiply transform p)
             (point3 2 1 7)))))

  (testing "Multiplying by the inverse of a translation matrix"
    (let [transform (translation 5 -3 2)
          inv (inverse transform)
          p (point3 -3 4 5)]
      (is (= (multiply inv p)
             (point3 -8 7 3)))))

  (testing "Translation does not affect vectors"
    (let [transform (translation 5 -3 2)
          v (vector3 -3 4 5)]
      (is (= (multiply transform v)
             v))))

  (testing "A scaling matrix applied to a point"
    (let [transform (scaling 2 3 4)
          p (point3 -4 6 8)]
      (is (= (multiply transform p)
             (point3 -8 18 32)))))

  (testing "A scaling matrix applied to a vector"
    (let [transform (scaling 2 3 4)
          v (vector3 -4 6 8)]
      (is (= (multiply transform v)
             (vector3 -8 18 32)))))

  (testing "Multiplying by the inverse of a scaling matrix"
    (let [transform (scaling 2 3 4)
          inv (inverse transform)
          v (vector3 -4 6 8)]
      (is (= (multiply inv v)
             (vector3 -2 2 2)))))

  (testing "Reflection is scaling by a negative value"
    (let [transform (scaling -1 1 1)
          p (point3 2 3 4)]
      (is (= (multiply transform p)
             (point3 -2 3 4)))))

  (testing "Rotating a point around the x axis"
    (let [p (point3 0 1 0)
          half-quarter (rotation-x (/ pi 4))
          full-quarter (rotation-x (/ pi 2))]
      (is (eq (multiply half-quarter p)
              (point3 0
                      (/ (sqrt 2) 2)
                      (/ (sqrt 2) 2))))
      (is (eq (multiply full-quarter p)
              (point3 0 0 1)))))

  (testing "The inverse of an x-rotation rotates in the opposite direction"
    (let [p (point3 0 1 0)
          half-quarter (rotation-x (/ pi 4))
          inv (inverse half-quarter)]
      (is (eq (multiply inv p)
              (point3 0
                      (/ (sqrt 2) 2)
                      (- (/ (sqrt 2) 2)))))))

  (testing "Rotating a point around the y axis"
    (let [p (point3 0 0 1)
          half-quarter (rotation-y (/ pi 4))
          full-quarter (rotation-y (/ pi 2))]
      (is (eq (multiply half-quarter p)
              (point3 (/ (sqrt 2) 2)
                      0
                      (/ (sqrt 2) 2))))
      (is (eq (multiply full-quarter p)
              (point3 1 0 0)))))

  (testing "Rotating a point around the z axis"
    (let [p (point3 0 1 0)
          half-quarter (rotation-z (/ pi 4))
          full-quarter (rotation-z (/ pi 2))]
      (is (eq (multiply half-quarter p)
              (point3 (- (/ (sqrt 2) 2))
                      (/ (sqrt 2) 2)
                      0)))
      (is (eq (multiply full-quarter p)
              (point3 -1 0 0)))))

  (testing "A shearing transformation moves x in proportion to y"
    (let [transform (shearing 1 0 0 0 0 0)
          p (point3 2 3 4)]
      (is (eq (multiply transform p)
              (point3 5 3 4)))))

  (testing "A shearing transformation moves x in proportion to z"
    (let [transform (shearing 0 1 0 0 0 0)
          p (point3 2 3 4)]
      (is (eq (multiply transform p)
              (point3 6 3 4)))))

  (testing "A shearing transformation moves y in proportion to x"
    (let [transform (shearing 0 0 1 0 0 0)
          p (point3 2 3 4)]
      (is (eq (multiply transform p)
              (point3 2 5 4)))))

  (testing "A shearing transformation moves y in proportion to z"
    (let [transform (shearing 0 0 0 1 0 0)
          p (point3 2 3 4)]
      (is (eq (multiply transform p)
              (point3 2 7 4)))))

  (testing "A shearing transformation moves z in proportion to x"
    (let [transform (shearing 0 0 0 0 1 0)
          p (point3 2 3 4)]
      (is (eq (multiply transform p)
              (point3 2 3 6)))))

  (testing "A shearing transformation moves z in proportion to y"
    (let [transform (shearing 0 0 0 0 0 1)
          p (point3 2 3 4)]
      (is (eq (multiply transform p)
              (point3 2 3 7)))))

  (testing "Individual transformations are applied in sequence"
    (let [p (point3 1 0 1)
          A (rotation-x (/ pi 2))
          B (scaling 5 5 5)
          C (translation 10 5 7)
          p2 (multiply A p)
          p3 (multiply B p2)
          p4 (multiply C p3)]
      (is (eq p2 (point3 1 -1 0)))
      (is (eq p3 (point3 5 -5 0)))
      (is (eq p4 (point3 15 0 7)))))

  (testing "Chained transformations must be applied in reverse order"
    (let [p (point3 1 0 1)
          A (rotation-x (/ pi 2))
          B (scaling 5 5 5)
          C (translation 10 5 7)
          T (reduce multiply [C B A])]
      (is (eq (multiply T p)
              (point3 15 0 7))))))
