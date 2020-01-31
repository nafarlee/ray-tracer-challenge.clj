(ns a.core-test
  (:require [clojure.test :refer :all]
            [a.core :refer :all]))

(deftest a-test
  (testing "A tuple with"
    (testing "w=1.0 is a point"
      (let [a (->Tuple 4.3 -4.2 3.1 1.0)]
        (is (= (:x a) 4.3))
        (is (= (:y a) -4.2))
        (is (= (:z a) 3.1))
        (is (= (:w a) 1.0))
        (is (point? a))
        (is (not (vector'? a)))))
    (testing "w=0.0 is a vector"
      (let [a (->Tuple 4.3 -4.2 3.1 0.0)]
        (is (= (:x a) 4.3))
        (is (= (:y a) -4.2))
        (is (= (:z a) 3.1))
        (is (= (:w a) 0.0))
        (is (not (point? a))
        (is (vector'? a))))))
  (testing "point creates tuples with w=1"
    (let [a (point 4 -4 3)]
      (is (= a (->Tuple 4 -4 3 1)))))
  (testing "vector creates tuples with w=0"
    (let [a (vector' 4 -4 3)]
      (is (= a (->Tuple 4 -4 3 0)))))
  (testing "Adding two tuples"
    (let [a1 (->Tuple 3 -2 5 1)
          a2 (->Tuple -2 3 1 0)]
      (is (= (add a1 a2)
             (->Tuple 1 1 6 1)))))
  (testing "Subtracting"
    (testing "two points"
      (let [p1 (point 3 2 1)
            p2 (point 5 6 7)]
        (is (= (subtract p1 p2)
               (vector' -2 -4 -6)))))
    (testing "a vector from a point"
      (let [p (point 3 2 1)
            v (vector' 5 6 7)]
        (is (= (subtract p v)
               (point -2 -4 -6)))))
    (testing "two vectors"
      (let [v1 (vector' 3 2 1)
            v2 (vector' 5 6 7)]
        (is (= (subtract v1 v2)
               (vector' -2 -4 -6)))))))
