(ns ray.tuples-test
  (:require
    [clojure.test :refer [deftest is testing]]
    pjstadig.humane-test-output
    [ray.math :refer [sqrt]]
    [ray.color :refer [color]]
    [ray.matrix
     :refer
     [add
      subtract
      eq
      hadamard
      negate
      scalar-multiply
      scalar-divide]]
    [ray.tuple :refer [normalize magnitude dot tuple]]
    [ray.point3 :refer [point3 point3?]]
    [ray.vector3 :refer [cross reflect vector3 vector3?]]))

(pjstadig.humane-test-output/activate!)

(deftest tuples
  (testing "A tuple with w=1.0 is a point"
    (let [[[x]
           [y]
           [z]
           [w] :as a] (tuple 4.3 -4.2 3.1 1.0)]
      (is (== x 4.3))
      (is (== y -4.2))
      (is (== z 3.1))
      (is (== w 1.0))
      (is (point3? a))
      (is (not (vector3? a)))))

  (testing "A tuple with w=0.0 is a vector"
    (let [[[x]
           [y]
           [z]
           [w] :as a] (tuple 4.3 -4.2 3.1 0.0)]
      (is (== x 4.3))
      (is (== y -4.2))
      (is (== z 3.1))
      (is (== w 0.0))
      (is (not (point3? a)))
      (is (vector3? a))))

  (testing "point creates tuples with w=1"
    (let [a (point3 4 -4 3)]
      (is (eq a (tuple 4 -4 3 1)))))

  (testing "vector creates tuples with w=0"
    (let [a (vector3 4 -4 3)]
      (is (eq a (tuple 4 -4 3 0)))))

  (testing "Adding two tuples"
    (let [a1 (tuple 3 -2 5 1)
          a2 (tuple -2 3 1 0)]
      (is (eq (add a1 a2) (tuple 1 1 6 1)))))

  (testing "Subtracting two points"
    (let [p1 (point3 3 2 1)
          p2 (point3 5 6 7)]
      (is (eq (subtract p1 p2) (vector3 -2 -4 -6)))))

  (testing "Subtracting a vector from a point"
    (let [p (point3 3 2 1)
          v (vector3 5 6 7)]
      (is (eq (subtract p v) (point3 -2 -4 -6)))))

  (testing "Subtracting two vectors"
    (let [v1 (vector3 3 2 1)
          v2 (vector3 5 6 7)]
      (is (eq (subtract v1 v2) (vector3 -2 -4 -6)))))

  (testing "Subtracting a vector from the zero vector"
    (let [zero (vector3 0 0 0)
          v (vector3 1 -2 3)]
      (is (eq (subtract zero v) (vector3 -1 2 -3)))))

  (testing "Negating a tuple"
    (let [a (tuple 1 -2 3 -4)]
      (is (eq (negate a) (tuple -1 2 -3 4)))))

  (testing "Multiplying a tuple by a scalar"
    (let [a (tuple 1 -2 3 -4)]
      (is (eq (scalar-multiply a 3.5) (tuple 3.5 -7.0 10.5 -14.0)))))

  (testing "Multiplying a tuple by a fraction"
    (let [a (tuple 1 -2 3 -4)]
      (is (eq (scalar-multiply a 0.5) (tuple 0.5 -1.0 1.5 -2.0)))))

  (testing "Dividing a tuple by a scalar"
    (let [a (tuple 1.0 -2.0 3.0 -4.0)]
      (is (eq (scalar-divide a 2) (tuple 0.5 -1.0 1.5 -2.0)))))

  (testing "Computing the magnitude of vector (1, 0, 0)"
    (let [v (vector3 1 0 0)]
      (is (== (magnitude v)
              1.0))))

  (testing "Computing the magnitude of vector (0, 1, 0)"
    (let [v (vector3 0 1 0)]
      (is (== (magnitude v)
              1.0))))

  (testing "Computing the magnitude of vector (0, 0, 1)"
    (let [v (vector3 0 0 1)]
      (is (== (magnitude v)
              1.0))))

  (testing "Computing the magnitude of vector (1, 2, 3)"
    (let [v (vector3 1 2 3)]
      (is (== (magnitude v)
              (sqrt 14)))))

  (testing "Computing the magnitude of vector (-1, -2, -3)"
    (let [v (vector3 -1 -2 -3)]
      (is (== (magnitude v)
              (sqrt 14)))))

  (testing "Normalizing (4, 0, 0) gives (1, 0, 0)"
    (let [v (vector3 4 0 0)]
      (is (eq (normalize v) (vector3 1.0 0.0 0.0)))))

  (testing "Normalizing (1, 2, 3)"
    (let [v (vector3 1 2 3)]
      (is (eq (normalize v)
              (vector3 (/ 1 (sqrt 14))
                       (/ 2 (sqrt 14))
                       (/ 3 (sqrt 14)))))))

  (testing "The magnitude of a normalized vector"
    (let [v (vector3 1 2 3)
          norm (normalize v)]
      (is (== (magnitude norm)
              1.0))))

  (testing "The dot product of two tuples"
    (let [a (vector3 1 2 3)
          b (vector3 2 3 4)]
      (is (== (dot a b)
              20))))

  (testing "The cross product of two vectors"
    (let [a (vector3 1 2 3)
          b (vector3 2 3 4)]
      (is (eq (cross a b) (vector3 -1 2 -1)))
      (is (eq (cross b a) (vector3 1 -2 1)))))

  (testing "Colors are (red, green, blue) tuples"
    (let [[[red]
           [green]
           [blue]] (color -0.5 0.4 1.7)]
      (is (== red -0.5))
      (is (== green 0.4))
      (is (== blue 1.7))))

  (testing "Adding colors"
    (let [c1 (color 0.9 0.6 0.75)
          c2 (color 0.7 0.1 0.25)]
      (is (eq (add c1 c2)
              (color 1.6 0.7 1.0)))))

  (testing "Subtracting colors"
    (let [c1 (color 0.9 0.6 0.75)
          c2 (color 0.7 0.1 0.25)]
      (is (eq (subtract c1 c2)
              (color 0.2 0.5 0.5)))))

  (testing "Multiplying a color by scalar"
    (let [c (color 0.2 0.3 0.4)]
      (is (eq (color 0.4 0.6 0.8)
              (scalar-multiply c 2)))))

  (testing "Multiplying colors"
    (let [c1 (color 1 0.2 0.4)
          c2 (color 0.9 1 0.1)]
      (is (eq (hadamard c1 c2)
              (color 0.9 0.2 0.04)))))

  (testing "Reflecting a vector approaching at 45°"
    (let [v (vector3 1 -1 0)
          n (vector3 0 1 0)
          r (reflect v n)]
      (is (eq r (vector3 1 1 0)))))

  (testing "Reflecting a vector off a slanted surface"
    (let [v    (vector3 0 -1 0)
          root (/ (sqrt 2) 2)
          n    (vector3 root root 0)
          r    (reflect v n)]
      (is (eq r (vector3 1 0 0))))))

