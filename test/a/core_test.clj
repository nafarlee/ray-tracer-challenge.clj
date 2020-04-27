(ns a.core-test
  (:require [clojure.test :refer :all]
            [a.core :refer :all]))

(deftest a-test
  (testing "A tuple with"
    (testing "w=1.0 is a point"
      (let [a (->Tuple 4.3 -4.2 3.1 1.0)]
        (is (== (:x a) 4.3))
        (is (== (:y a) -4.2))
        (is (== (:z a) 3.1))
        (is (== (:w a) 1.0))
        (is (point? a))
        (is (not (vector'? a)))))
    (testing "w=0.0 is a vector"
      (let [a (->Tuple 4.3 -4.2 3.1 0.0)]
        (is (== (:x a) 4.3))
        (is (== (:y a) -4.2))
        (is (== (:z a) 3.1))
        (is (== (:w a) 0.0))
        (is (not (point? a))
        (is (vector'? a))))))
  (testing "point creates tuples with w=1"
    (let [a (point 4 -4 3)]
      (is (eq a (->Tuple 4 -4 3 1)))))
  (testing "vector creates tuples with w=0"
    (let [a (vector' 4 -4 3)]
      (is (eq a (->Tuple 4 -4 3 0)))))
  (testing "Adding two tuples"
    (let [a1 (->Tuple 3 -2 5 1)
          a2 (->Tuple -2 3 1 0)]
      (is (eq (add a1 a2)
              (->Tuple 1 1 6 1)))))
  (testing "Subtracting"
    (testing "two points"
      (let [p1 (point 3 2 1)
            p2 (point 5 6 7)]
        (is (eq (subtract p1 p2)
                (vector' -2 -4 -6)))))
    (testing "a vector from a point"
      (let [p (point 3 2 1)
            v (vector' 5 6 7)]
        (is (eq (subtract p v)
                (point -2 -4 -6)))))
    (testing "two vectors"
      (let [v1 (vector' 3 2 1)
            v2 (vector' 5 6 7)]
        (is (eq (subtract v1 v2)
                (vector' -2 -4 -6)))))
    (testing "a vector from the zero vector"
      (let [zero (vector' 0 0 0)
            v (vector' 1 -2 3)]
        (is (eq (subtract zero v)
                (vector' -1 2 -3))))))
  (testing "Negating a tuple"
    (let [a (->Tuple 1 -2 3 -4)]
      (is (eq (negate a)
              (->Tuple -1 2 -3 4)))))
  (testing "Multiplying"
    (testing "a tuple by a scalar"
      (let [a (->Tuple 1 -2 3 -4)]
        (is (eq (multiply a 3.5)
                (->Tuple 3.5 -7.0 10.5 -14.0)))))
    (testing "a tuple by a fraction"
      (let [a (->Tuple 1 -2 3 -4)]
        (is (eq (multiply a 0.5)
                (->Tuple 0.5 -1.0 1.5 -2.0))))))
  (testing "Dividing a tuple by a scalar"
    (let [a (->Tuple 1.0 -2.0 3.0 -4.0)]
      (is (eq (divide a 2)
              (->Tuple 0.5 -1.0 1.5 -2.0)))))
  (testing "Computing the magnitude of vector"
    (testing "(1, 0, 0)"
      (let [v (vector' 1 0 0)]
        (is (== (magnitude v)
                1.0))))
    (testing "(0, 1, 0)"
      (let [v (vector' 0 1 0)]
        (is (== (magnitude v)
                1.0))))
    (testing "(0, 0, 1)"
      (let [v (vector' 0 0 1)]
        (is (== (magnitude v)
                1.0))))
    (testing "(1, 2, 3)"
      (let [v (vector' 1 2 3)]
        (is (== (magnitude v)
                (Math/sqrt 14)))))
    (testing "(-1, -2, -3)"
      (let [v (vector' -1 -2 -3)]
        (is (== (magnitude v)
                (Math/sqrt 14))))))
  (testing "Normalizing"
    (testing "(4, 0, 0) gives (1, 0, 0)"
      (let [v (vector' 4 0 0)]
        (is (eq (normalize v)
                (vector' 1.0 0.0 0.0)))))
    (testing "(1, 2, 3)"
      (let [v (vector' 1 2 3)]
        (is (eq (normalize v)
                (vector' (/ 1 (Math/sqrt 14))
                         (/ 2 (Math/sqrt 14))
                         (/ 3 (Math/sqrt 14))))))))
  (testing "The magnitude of a normalized vector"
    (let [v (vector' 1 2 3)
          norm (normalize v)]
      (is (== (magnitude norm)
              1.0))))
  (testing "The dot product of two tuples"
    (let [a (vector' 1 2 3)
          b (vector' 2 3 4)]
      (is (== (dot a b)
              20))))
  (testing "The cross product of two vectors"
    (let [a (vector' 1 2 3)
          b (vector' 2 3 4)]
      (is (eq (cross a b)
              (vector' -1 2 -1)))
      (is (eq (cross b a)
              (vector' 1 -2 1)))))
  (testing "Colors are (red, green, blue) tuples"
    (let [{:keys [red green blue]} (->Color -0.5 0.4 1.7)]
      (is (== red -0.5))
          (is (== green 0.4))
          (is (== blue 1.7))))
  (testing "Adding colors"
    (let [c1 (->Color 0.9 0.6 0.75)
          c2 (->Color 0.7 0.1 0.25)]
      (is (eq (add c1 c2)
              (->Color 1.6 0.7 1.0)))))
  (testing "Subtracting colors"
    (let [c1 (->Color 0.9 0.6 0.75)
          c2 (->Color 0.7 0.1 0.25)]
      (is (eq (subtract c1 c2)
              (->Color 0.2 0.5 0.5)))))
  (testing "Multiplying a color by scalar"
    (let [c (->Color 0.2 0.3 0.4)]
      (is (eq (->Color 0.4 0.6 0.8)
              (multiply c 2))))))
