(ns a.core-test
  (:require [clojure.test :refer :all]
            [a.core :refer :all]))

(deftest a-test
  (testing "A tuple with"
    (testing "w=1.0 is a point"
      (let [t (->Tuple 4.3 -4.2 3.1 1.0)]
        (is (= (:x t) 4.3))
        (is (= (:y t) -4.2))
        (is (= (:z t) 3.1))
        (is (= (:w t) 1.0))
        (is (point? t))
        (is (not (vector'? t)))))
    (testing "w=0.0 is a vector"
      (let [t (->Tuple 4.3 -4.2 3.1 0.0)]
        (is (= (:x t) 4.3))
        (is (= (:y t) -4.2))
        (is (= (:z t) 3.1))
        (is (= (:w t) 0.0))
        (is (not (point? t))
        (is (vector'? t))))))
  (testing "point creates tuples with w=1"
    (let [t (point 4 -4 3)]
      (is (= t (->Tuple 4 -4 3 1)))))
  (testing "vector creates tuples with w=0"
    (let [t (vector' 4 -4 3)]
      (is (= t (->Tuple 4 -4 3 0))))))
