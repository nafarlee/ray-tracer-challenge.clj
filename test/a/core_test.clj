(ns a.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [a.core :as ray]))

(deftest a-test
  (testing "A tuple with w=1.0 is a point"
    (let [a (ray/->Tuple 4.3 -4.2 3.1 1.0)]
      (is (== (:x a) 4.3))
      (is (== (:y a) -4.2))
      (is (== (:z a) 3.1))
      (is (== (:w a) 1.0))
      (is (ray/point? a))
      (is (not (ray/vector'? a)))))

  (testing "A tuple with w=0.0 is a vector"
    (let [a (ray/->Tuple 4.3 -4.2 3.1 0.0)]
      (is (== (:x a) 4.3))
      (is (== (:y a) -4.2))
      (is (== (:z a) 3.1))
      (is (== (:w a) 0.0))
      (is (not (ray/point? a))
      (is (ray/vector'? a)))))

  (testing "point creates tuples with w=1"
    (let [a (ray/point 4 -4 3)]
      (is (ray/eq a (ray/->Tuple 4 -4 3 1)))))

  (testing "vector creates tuples with w=0"
    (let [a (ray/vector' 4 -4 3)]
      (is (ray/eq a (ray/->Tuple 4 -4 3 0)))))

  (testing "Adding two tuples"
    (let [a1 (ray/->Tuple 3 -2 5 1)
          a2 (ray/->Tuple -2 3 1 0)]
      (is (ray/eq (ray/add a1 a2)
                  (ray/->Tuple 1 1 6 1)))))

  (testing "Subtracting two points"
    (let [p1 (ray/point 3 2 1)
          p2 (ray/point 5 6 7)]
      (is (ray/eq (ray/subtract p1 p2)
                  (ray/vector' -2 -4 -6)))))

  (testing "Subtracting a vector from a point"
    (let [p (ray/point 3 2 1)
          v (ray/vector' 5 6 7)]
      (is (ray/eq (ray/subtract p v)
                  (ray/point -2 -4 -6)))))

  (testing "Subtracting two vectors"
    (let [v1 (ray/vector' 3 2 1)
          v2 (ray/vector' 5 6 7)]
      (is (ray/eq (ray/subtract v1 v2)
                  (ray/vector' -2 -4 -6)))))

  (testing "Subtracting a vector from the zero vector"
    (let [zero (ray/vector' 0 0 0)
          v (ray/vector' 1 -2 3)]
      (is (ray/eq (ray/subtract zero v)
                  (ray/vector' -1 2 -3)))))

  (testing "Negating a tuple"
    (let [a (ray/->Tuple 1 -2 3 -4)]
      (is (ray/eq (ray/negate a)
                  (ray/->Tuple -1 2 -3 4)))))

  (testing "Multiplying a tuple by a scalar"
    (let [a (ray/->Tuple 1 -2 3 -4)]
      (is (ray/eq (ray/multiply a 3.5)
                  (ray/->Tuple 3.5 -7.0 10.5 -14.0)))))

  (testing "Multiplying a tuple by a fraction"
    (let [a (ray/->Tuple 1 -2 3 -4)]
      (is (ray/eq (ray/multiply a 0.5)
                  (ray/->Tuple 0.5 -1.0 1.5 -2.0)))))

  (testing "Dividing a tuple by a scalar"
    (let [a (ray/->Tuple 1.0 -2.0 3.0 -4.0)]
      (is (ray/eq (ray/divide a 2)
                  (ray/->Tuple 0.5 -1.0 1.5 -2.0)))))

  (testing "Computing the magnitude of vector (1, 0, 0)"
    (let [v (ray/vector' 1 0 0)]
      (is (== (ray/magnitude v)
              1.0))))

  (testing "Computing the magnitude of vector (0, 1, 0)"
    (let [v (ray/vector' 0 1 0)]
      (is (== (ray/magnitude v)
              1.0))))

  (testing "Computing the magnitude of vector (0, 0, 1)"
    (let [v (ray/vector' 0 0 1)]
      (is (== (ray/magnitude v)
              1.0))))

  (testing "Computing the magnitude of vector (1, 2, 3)"
    (let [v (ray/vector' 1 2 3)]
      (is (== (ray/magnitude v)
              (Math/sqrt 14)))))

  (testing "Computing the magnitude of vector (-1, -2, -3)"
    (let [v (ray/vector' -1 -2 -3)]
      (is (== (ray/magnitude v)
              (Math/sqrt 14)))))

  (testing "Normalizing (4, 0, 0) gives (1, 0, 0)"
    (let [v (ray/vector' 4 0 0)]
      (is (ray/eq (ray/normalize v)
                  (ray/vector' 1.0 0.0 0.0)))))

  (testing "Normalizing (1, 2, 3)"
    (let [v (ray/vector' 1 2 3)]
      (is (ray/eq (ray/normalize v)
              (ray/vector' (/ 1 (Math/sqrt 14))
                           (/ 2 (Math/sqrt 14))
                           (/ 3 (Math/sqrt 14)))))))

  (testing "The magnitude of a normalized vector"
    (let [v (ray/vector' 1 2 3)
          norm (ray/normalize v)]
      (is (== (ray/magnitude norm)
              1.0))))

  (testing "The dot product of two tuples"
    (let [a (ray/vector' 1 2 3)
          b (ray/vector' 2 3 4)]
      (is (== (ray/dot a b)
              20))))

  (testing "The cross product of two vectors"
    (let [a (ray/vector' 1 2 3)
          b (ray/vector' 2 3 4)]
      (is (ray/eq (ray/cross a b)
                  (ray/vector' -1 2 -1)))
      (is (ray/eq (ray/cross b a)
                  (ray/vector' 1 -2 1)))))

  (testing "Colors are (red, green, blue) tuples"
    (let [{:keys [red green blue]} (ray/->Color -0.5 0.4 1.7)]
      (is (== red -0.5))
          (is (== green 0.4))
          (is (== blue 1.7))))

  (testing "Adding colors"
    (let [c1 (ray/->Color 0.9 0.6 0.75)
          c2 (ray/->Color 0.7 0.1 0.25)]
      (is (ray/eq (ray/add c1 c2)
                  (ray/->Color 1.6 0.7 1.0)))))

  (testing "Subtracting colors"
    (let [c1 (ray/->Color 0.9 0.6 0.75)
          c2 (ray/->Color 0.7 0.1 0.25)]
      (is (ray/eq (ray/subtract c1 c2)
                  (ray/->Color 0.2 0.5 0.5)))))

  (testing "Multiplying a color by scalar"
    (let [c (ray/->Color 0.2 0.3 0.4)]
      (is (ray/eq (ray/->Color 0.4 0.6 0.8)
                  (ray/multiply c 2)))))

  (testing "Multiplying colors"
    (let [c1 (ray/->Color 1 0.2 0.4)
          c2 (ray/->Color 0.9 1 0.1)]
      (is (ray/eq (ray/hadamard c1 c2)
                  (ray/->Color 0.9 0.2 0.04)))))

  (testing "Creating a canvas"
    (let [c (ray/canvas 10 20)]
      (is (== (:width c) 10))
      (is (== (:height c) 20))
      (is (every? (partial ray/eq (ray/->Color 0 0 0))
                  (:pixels c)))))

  (testing "Writing pixels to a canvas"
    (let [c (ray/canvas 10 20)
          red (ray/->Color 1 0 0)]
      (is (ray/eq red
                  (-> c
                      (ray/write-pixel 2 3 red)
                      (ray/pixel-at 2 3)))))))
