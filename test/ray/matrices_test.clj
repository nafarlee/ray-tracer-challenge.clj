(ns ray.matrices-test
  (:require
   [clojure.test :refer [deftest is testing]]
   pjstadig.humane-test-output
   [ray.point3 :refer [point3]]
   [ray.tuple :refer [tuple]]
   [ray.matrix
    :refer
    [at
     id
     minor
     multiply
     transpose
     determinant
     submatrix
     cofactor
     invertible?
     fmap
     inverse]]))

(pjstadig.humane-test-output/activate!)

(deftest matrices.feature
  (testing "Constructing and inspecting a 4x4 matrix"
    (let [M [[1    2    3    4]
             [5.5  6.5  7.5  8.5]
             [9    10   11   12]
             [13.5 14.5 15.5 16.5]]]
      (is (== 1 (at M 0 0)))
      (is (== 4 (at M 0 3)))
      (is (== 5.5 (at M 1 0)))
      (is (== 7.5 (at M 1 2)))
      (is (== 11 (at M 2 2)))
      (is (== 13.5 (at M 3 0)))
      (is (== 15.5 (at M 3 2)))))

  (testing "A 2x2 matrix ought to be representable"
    (let [M [[-3  5]
             [ 1 -2]]]
      (is (== -3 (at M 0 0)))
      (is (== 5 (at M 0 1)))
      (is (== 1 (at M 1 0)))
      (is (== -2 (at M 1 1)))))

  (testing "A 3x3 matrix ought to be representable"
    (let [M [[-3   5  0]
             [ 1  -2 -7]
             [ 0   1  1]]]
      (is (== -3 (at M 0 0)))
      (is (== -2 (at M 1 1)))
      (is (== 1 (at M 2 2)))))

  (testing "Matrix equality with identical matrices"
    (let [A [[1 2 3 4]
             [5 6 7 8]
             [9 8 7 6]
             [5 4 3 2]]
          B [[1 2 3 4]
             [5 6 7 8]
             [9 8 7 6]
             [5 4 3 2]]]
      (is (= A B))))

  (testing "Matrix equality with different matrices"
    (let [A [[1 2 3 4]
             [5 6 7 8]
             [9 8 7 6]
             [5 4 3 2]]
          B [[2 3 4 5]
             [6 7 8 9]
             [8 7 6 5]
             [4 3 2 1]]]
      (is (not= A B))))

  (testing "Multiplying two matrices"
    (let [A [[1 2 3 4]
             [5 6 7 8]
             [9 8 7 6]
             [5 4 3 2]]
          B [[-2 1 2  3]
             [ 3 2 1 -1]
             [ 4 3 6  5]
             [ 1 2 7  8]]
          AB [[20 22 50  48]
              [44 54 114 108]
              [40 58 110 102]
              [16 26 46  42]]]
      (is (= (multiply A B) AB))))

  (testing "A matrix multiplied by a tuple"
    (let [A [[1 2 3 4]
             [2 4 4 2]
             [8 6 4 1]
             [0 0 0 1]]
          b (point3 1.0 2.0 3.0)
          Ab (point3 18.0 24.0 33.0)]
      (is (= (multiply A b) Ab))))

  (testing "Multiplying a matrix by the identity matrix"
    (let [A [[0 1 2  4]
             [1 2 4  8]
             [2 4 8  16]
             [4 8 16 32]]]
      (is (= (multiply A id) A))))

  (testing "Multiplying the identity matrix by a tuple"
    (let [a (tuple 1 2 3 4)]
      (is (= (multiply id a) a))))

  (testing "Transposing a matrix"
    (let [A [[0 9 3 0]
             [9 8 0 8]
             [1 8 5 3]
             [0 0 5 8]]]
      (is (= (transpose A) [[0 9 1 0]
                            [9 8 8 0]
                            [3 0 5 5]
                            [0 8 3 8]]))))

  (testing "Transposing the identity matrix"
    (let [A (transpose id)]
      (is (= A id))))

  (testing "Calculating the determinant of a 2x2 matrix"
    (let [A [[1 5]
             [-3 2]]]
      (is (= (determinant A) 17))))

  (testing "A submatrix of a 3x3 matrix is a 2x2 matrix"
    (let [A [[ 1 5  8]
             [-3 2  7]
             [ 0 6 -3]]]
      (is (= (submatrix A 0 2) [[-3 2]
                                [ 0 6]]))))

  (testing "A submatrix of a 4x4 matrix is a 3x3 matrix"
    (let [A [[-6 1 1 6]
             [-8 5 8 6]
             [-1 0 8 2]
             [-7 1 -1 1]]]
      (is (= (submatrix A 2 1) [[-6  1 6]
                                [-8  8 6]
                                [-7 -1 1]]))))

  (testing "Calculating a minor of a 3x3 matrix"
    (let [A [[3  5  0]
             [2 -1 -7]
             [6 -1  5]]
          B (submatrix A 1 0)]
      (is (= 25 (minor A 1 0) (determinant B)))))

  (testing "Calculating a cofactor of a 3x3 matrix"
    (let [A [[3  5  0]
             [2 -1 -7]
             [6 -1  5]]]
      (is (= -12 (minor A 0 0)))
      (is (= -12 (cofactor A 0 0)))
      (is (= 25 (minor A 1 0)))
      (is (= -25 (cofactor A 1 0)))))

  (testing "Calculating the determinant of a 3x3 matrix"
    (let [A [[ 1 2  6]
             [-5 8 -4]
             [ 2 6  4]]]
      (is (= 56 (cofactor A 0 0)))
      (is (= 12 (cofactor A 0 1)))
      (is (= -46 (cofactor A 0 2)))
      (is (= -196 (determinant A)))))

  (testing "Calculating the determinant of a 4x4 matrix"
    (let [A [[-2 -8  3  5]
             [-3  1  7  3]
             [ 1  2 -9  6]
             [-6  7  7 -9]]]
      (is (= 690 (cofactor A 0 0)))
      (is (= 447 (cofactor A 0 1)))
      (is (= 210 (cofactor A 0 2)))
      (is (= 51 (cofactor A 0 3)))
      (is (= -4071 (determinant A)))))

  (testing "Testing an invertible matrix for invertibility"
    (let [A [[6  4 4  4]
             [5  5 7  6]
             [4 -9 3 -7]
             [9  1 7 -6]]]
      (is (= -2120 (determinant A)))
      (is (invertible? A))))

  (testing "Testing a noninvertible matrix for invertibility"
    (let [A [[-4  2 -2 -3]
             [ 9  6  2  6]
             [ 0 -5  1 -5]
             [ 0  0  0  0]]]
      (is (zero? (determinant A)))
      (is (not (invertible? A)))))

  (testing "Calculating the inverse of a matrix"
    (let [A [[-5 2 6 -8]
             [1 -5 1 8]
             [7 7 -6 -7]
             [1 -3 7 4]]
          B (inverse A)]
      (is (= 532 (determinant A)))
      (is (= -160 (cofactor A 2 3)))
      (is (= -160/532 (at B 3 2)))
      (is (= 105 (cofactor A 3 2)))
      (is (= 105/532 (at B 2 3)))
      (is (= [[ 0.21805  0.45113  0.24060 -0.04511]
              [-0.80827 -1.45677 -0.44361  0.52068]
              [-0.07895 -0.22368 -0.05263  0.19737]
              [-0.52256 -0.81391 -0.30075  0.30639]]
             (fmap #(->> % double (format "%.5f") Double.)
                   B)))))

  (testing "Calculating the inverse of another matrix"
    (let [A [[8 -5 9 2]
             [7 5 6 1]
             [-6 0 9 6]
             [-3 0 -9 -4]]]
      (is (= [[-0.15385 -0.15385 -0.28205 -0.53846]
              [-0.07692  0.12308  0.02564  0.03077]
              [ 0.35897  0.35897  0.43590  0.92308]
              [-0.69231 -0.69231 -0.76923 -1.92308]]
             (fmap #(->> % double (format "%.5f") Double.)
                   (inverse A))))))

  (testing "Calculating the inverse of a third matrix"
    (let [A [[ 9  3  0  9]
             [-5 -2 -6 -3]
             [-4  9  6  4]
             [-7  6  6  2]]]
      (is (= [[-0.04074 -0.07778  0.14444 -0.22222]
              [-0.07778  0.03333  0.36667 -0.33333]
              [-0.02901 -0.14630 -0.10926  0.12963]
              [ 0.17778  0.06667 -0.26667  0.33333]]
             (fmap #(->> % double (format "%.5f") Double.)
                   (inverse A))))))

  (testing "Multiplying a product by its inverse"
    (let [A [[ 3 -9  7  3]
             [ 3 -8  2 -9]
             [-4  4  4  1]
             [-6  5 -1  1]]
          B [[8  2 2 2]
             [3 -1 7 0]
             [7  0 5 4]
             [6 -2 0 5]]
          C (multiply A B)]
      (is (= (multiply C (inverse B))
             A)))))
