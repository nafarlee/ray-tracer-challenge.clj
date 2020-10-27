(ns ray.core-test
  (:require
   [clojure.string :as st]
   [clojure.test :refer [deftest is testing]]
   [ray.shape :refer [intersect sphere]]
   [ray.ray :refer [->ray position]]
   [ray.math :refer [pi sqrt]]
   [ray.matrix :as matrix]
   [ray.tuple :as tuple]
   [ray.color :as rc]
   [ray.canvas :as rcan]
   [ray.ppm :as rp]
   [ray.string :as rs]))

(deftest ray-tracer-challenge-tests

(testing "A tuple with w=1.0 is a point"
  (let [[[x]
         [y]
         [z]
         [w] :as a] (tuple/tuple 4.3 -4.2 3.1 1.0)]
    (is (== x 4.3))
    (is (== y -4.2))
    (is (== z 3.1))
    (is (== w 1.0))
    (is (tuple/point? a))
    (is (not (tuple/vector'? a)))))

(testing "A tuple with w=0.0 is a vector"
  (let [[[x]
         [y]
         [z]
         [w] :as a] (tuple/tuple 4.3 -4.2 3.1 0.0)]
    (is (== x 4.3))
    (is (== y -4.2))
    (is (== z 3.1))
    (is (== w 0.0))
    (is (not (tuple/point? a))
    (is (tuple/vector'? a)))))

(testing "point creates tuples with w=1"
  (let [a (tuple/point 4 -4 3)]
    (is (tuple/eq a (tuple/tuple 4 -4 3 1)))))

(testing "vector creates tuples with w=0"
  (let [a (tuple/vector' 4 -4 3)]
    (is (tuple/eq a (tuple/tuple 4 -4 3 0)))))

(testing "Adding two tuples"
  (let [a1 (tuple/tuple 3 -2 5 1)
        a2 (tuple/tuple -2 3 1 0)]
    (is (tuple/eq (tuple/add a1 a2)
                (tuple/tuple 1 1 6 1)))))

(testing "Subtracting two points"
  (let [p1 (tuple/point 3 2 1)
        p2 (tuple/point 5 6 7)]
    (is (tuple/eq (tuple/subtract p1 p2)
                (tuple/vector' -2 -4 -6)))))

(testing "Subtracting a vector from a point"
  (let [p (tuple/point 3 2 1)
        v (tuple/vector' 5 6 7)]
    (is (tuple/eq (tuple/subtract p v)
                (tuple/point -2 -4 -6)))))

(testing "Subtracting two vectors"
  (let [v1 (tuple/vector' 3 2 1)
        v2 (tuple/vector' 5 6 7)]
    (is (tuple/eq (tuple/subtract v1 v2)
                (tuple/vector' -2 -4 -6)))))

(testing "Subtracting a vector from the zero vector"
  (let [zero (tuple/vector' 0 0 0)
        v (tuple/vector' 1 -2 3)]
    (is (tuple/eq (tuple/subtract zero v)
                (tuple/vector' -1 2 -3)))))

(testing "Negating a tuple"
  (let [a (tuple/tuple 1 -2 3 -4)]
    (is (tuple/eq (tuple/negate a)
                (tuple/tuple -1 2 -3 4)))))

(testing "Multiplying a tuple by a scalar"
  (let [a (tuple/tuple 1 -2 3 -4)]
    (is (tuple/eq (tuple/multiply a 3.5)
                (tuple/tuple 3.5 -7.0 10.5 -14.0)))))

(testing "Multiplying a tuple by a fraction"
  (let [a (tuple/tuple 1 -2 3 -4)]
    (is (tuple/eq (tuple/multiply a 0.5)
                (tuple/tuple 0.5 -1.0 1.5 -2.0)))))

(testing "Dividing a tuple by a scalar"
  (let [a (tuple/tuple 1.0 -2.0 3.0 -4.0)]
    (is (tuple/eq (tuple/divide a 2)
                (tuple/tuple 0.5 -1.0 1.5 -2.0)))))

(testing "Computing the magnitude of vector (1, 0, 0)"
  (let [v (tuple/vector' 1 0 0)]
    (is (== (tuple/magnitude v)
            1.0))))

(testing "Computing the magnitude of vector (0, 1, 0)"
  (let [v (tuple/vector' 0 1 0)]
    (is (== (tuple/magnitude v)
            1.0))))

(testing "Computing the magnitude of vector (0, 0, 1)"
  (let [v (tuple/vector' 0 0 1)]
    (is (== (tuple/magnitude v)
            1.0))))

(testing "Computing the magnitude of vector (1, 2, 3)"
  (let [v (tuple/vector' 1 2 3)]
    (is (== (tuple/magnitude v)
            (sqrt 14)))))

(testing "Computing the magnitude of vector (-1, -2, -3)"
  (let [v (tuple/vector' -1 -2 -3)]
    (is (== (tuple/magnitude v)
            (sqrt 14)))))

(testing "Normalizing (4, 0, 0) gives (1, 0, 0)"
  (let [v (tuple/vector' 4 0 0)]
    (is (tuple/eq (tuple/normalize v)
                (tuple/vector' 1.0 0.0 0.0)))))

(testing "Normalizing (1, 2, 3)"
  (let [v (tuple/vector' 1 2 3)]
    (is (tuple/eq (tuple/normalize v)
            (tuple/vector' (/ 1 (sqrt 14))
                         (/ 2 (sqrt 14))
                         (/ 3 (sqrt 14)))))))

(testing "The magnitude of a normalized vector"
  (let [v (tuple/vector' 1 2 3)
        norm (tuple/normalize v)]
    (is (== (tuple/magnitude norm)
            1.0))))

(testing "The dot product of two tuples"
  (let [a (tuple/vector' 1 2 3)
        b (tuple/vector' 2 3 4)]
    (is (== (tuple/dot a b)
            20))))

(testing "The cross product of two vectors"
  (let [a (tuple/vector' 1 2 3)
        b (tuple/vector' 2 3 4)]
    (is (tuple/eq (tuple/cross a b)
                (tuple/vector' -1 2 -1)))
    (is (tuple/eq (tuple/cross b a)
                (tuple/vector' 1 -2 1)))))

(testing "Colors are (red, green, blue) tuples"
  (let [[[red]
         [green]
         [blue]] (rc/color -0.5 0.4 1.7)]
    (is (== red -0.5))
    (is (== green 0.4))
    (is (== blue 1.7))))

(testing "Adding colors"
  (let [c1 (rc/color 0.9 0.6 0.75)
        c2 (rc/color 0.7 0.1 0.25)]
    (is (tuple/eq (tuple/add c1 c2)
                (rc/color 1.6 0.7 1.0)))))

(testing "Subtracting colors"
  (let [c1 (rc/color 0.9 0.6 0.75)
        c2 (rc/color 0.7 0.1 0.25)]
    (is (tuple/eq (tuple/subtract c1 c2)
                (rc/color 0.2 0.5 0.5)))))

(testing "Multiplying a color by scalar"
  (let [c (rc/color 0.2 0.3 0.4)]
    (is (tuple/eq (rc/color 0.4 0.6 0.8)
                (tuple/multiply c 2)))))

(testing "Multiplying colors"
  (let [c1 (rc/color 1 0.2 0.4)
        c2 (rc/color 0.9 1 0.1)]
    (is (tuple/eq (tuple/hadamard c1 c2)
                (rc/color 0.9 0.2 0.04)))))

(testing "Creating a canvas"
  (let [c (rcan/canvas 10 20)]
    (is (== (::rcan/width c) 10))
    (is (== (::rcan/height c) 20))
    (is (every? (partial tuple/eq (rc/color 0 0 0))
                (::rcan/pixels c)))))

(testing "Writing pixels to a canvas"
  (let [c (rcan/canvas 10 20)
        red (rc/color 1 0 0)]
    (is (tuple/eq red
                (-> c
                    (rcan/write-pixel 2 3 red)
                    (rcan/pixel-at 2 3))))))

(testing "Constructing the PPM header"
  (let [c (rcan/canvas 5 3)
        ppm (rp/canvas->ppm c)
        header (->> ppm (st/split-lines) (take 3) (st/join "\n"))]
    (is (= header (rs/$ "
                        P3
                        5 3
                        255
                        ")))))

(testing "Constructing the PPM pixel data"
  (let [c (rcan/canvas 5 3)
        c1 (rc/color 1.5 0 0)
        c2 (rc/color 0 0.5 0)
        c3 (rc/color -0.5 0 1)]
    (is (= (as-> c %
               (rcan/write-pixel % 0 0 c1)
               (rcan/write-pixel % 2 1 c2)
               (rcan/write-pixel % 4 2 c3)
               (rp/canvas->ppm %)
               (st/split-lines %)
               (drop 3 %)
               (take 3 %)
               (st/join "\n" %))
           (rs/$ "
                 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 128 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
                 ")))))

(testing "Splitting long lines in PPM files"
  (let [c (rcan/canvas 10 2)
        color (rc/color 1 0.8 0.6)]
    (is (= (as-> c $
                 (reduce (fn [can [x y]] (rcan/write-pixel can x y color))
                         $
                         (for [x (range 10), y (range 2)] [x y]))
                 (rp/canvas->ppm $)
                 (st/split-lines $)
                 (drop 3 $)
                 (take 4 $)
                 (st/join "\n" $))
           (rs/$ "
                 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
                 153 255 204 153 255 204 153 255 204 153 255 204 153
                 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
                 153 255 204 153 255 204 153 255 204 153 255 204 153
                 ")))))

(testing "PPM files are terminated by a newline character"
  (let [c (rcan/canvas 5 3)]
    (is (st/ends-with? (rp/canvas->ppm c)
                       "\n"))))

(testing "Constructing and inspecting a 4x4 matrix"
  (let [M [[1    2    3    4]
           [5.5  6.5  7.5  8.5]
           [9    10   11   12]
           [13.5 14.5 15.5 16.5]]]
    (is (== 1 (matrix/at M 0 0)))
    (is (== 4 (matrix/at M 0 3)))
    (is (== 5.5 (matrix/at M 1 0)))
    (is (== 7.5 (matrix/at M 1 2)))
    (is (== 11 (matrix/at M 2 2)))
    (is (== 13.5 (matrix/at M 3 0)))
    (is (== 15.5 (matrix/at M 3 2)))))

(testing "A 2x2 matrix ought to be representable"
  (let [M [[-3  5]
           [ 1 -2]]]
    (is (== -3 (matrix/at M 0 0)))
    (is (== 5 (matrix/at M 0 1)))
    (is (== 1 (matrix/at M 1 0)))
    (is (== -2 (matrix/at M 1 1)))))

(testing "A 3x3 matrix ought to be representable"
  (let [M [[-3   5  0]
           [ 1  -2 -7]
           [ 0   1  1]]]
    (is (== -3 (matrix/at M 0 0)))
    (is (== -2 (matrix/at M 1 1)))
    (is (== 1 (matrix/at M 2 2)))))

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
    (is (= (matrix/multiply A B) AB))))

(testing "A matrix multiplied by a tuple"
  (let [A [[1 2 3 4]
           [2 4 4 2]
           [8 6 4 1]
           [0 0 0 1]]
        b (tuple/point 1.0 2.0 3.0)
        Ab (tuple/point 18.0 24.0 33.0)]
    (is (= (matrix/multiply A b) Ab))))

(testing "Multiplying a matrix by the identity matrix"
  (let [A [[0 1 2  4]
           [1 2 4  8]
           [2 4 8  16]
           [4 8 16 32]]]
    (is (= (matrix/multiply A matrix/id) A))))

(testing "Multiplying the identity matrix by a tuple"
  (let [a (tuple/tuple 1 2 3 4)]
    (is (= (matrix/multiply matrix/id a) a))))

(testing "Transposing a matrix"
  (let [A [[0 9 3 0]
           [9 8 0 8]
           [1 8 5 3]
           [0 0 5 8]]]
    (is (= (matrix/transpose A) [[0 9 1 0]
                                 [9 8 8 0]
                                 [3 0 5 5]
                                 [0 8 3 8]]))))

(testing "Transposing the identity matrix"
  (let [A (matrix/transpose matrix/id)]
    (is (= A matrix/id))))

(testing "Calculating the determinant of a 2x2 matrix"
  (let [A [[1 5]
           [-3 2]]]
    (is (= (matrix/determinant A) 17))))

(testing "A submatrix of a 3x3 matrix is a 2x2 matrix"
  (let [A [[ 1 5  8]
           [-3 2  7]
           [ 0 6 -3]]]
    (is (= (matrix/submatrix A 0 2) [[-3 2]
                                     [ 0 6]]))))

(testing "A submatrix of a 4x4 matrix is a 3x3 matrix"
  (let [A [[-6 1 1 6]
           [-8 5 8 6]
           [-1 0 8 2]
           [-7 1 -1 1]]]
    (is (= (matrix/submatrix A 2 1) [[-6  1 6]
                                     [-8  8 6]
                                     [-7 -1 1]]))))

(testing "Calculating a minor of a 3x3 matrix"
  (let [A [[3  5  0]
           [2 -1 -7]
           [6 -1  5]]
        B (matrix/submatrix A 1 0)]
    (is (= 25 (matrix/minor A 1 0) (matrix/determinant B)))))

(testing "Calculating a cofactor of a 3x3 matrix"
  (let [A [[3  5  0]
           [2 -1 -7]
           [6 -1  5]]]
    (is (= -12 (matrix/minor A 0 0)))
    (is (= -12 (matrix/cofactor A 0 0)))
    (is (= 25 (matrix/minor A 1 0)))
    (is (= -25 (matrix/cofactor A 1 0)))))

(testing "Calculating the determinant of a 3x3 matrix"
  (let [A [[ 1 2  6]
           [-5 8 -4]
           [ 2 6  4]]]
    (is (= 56 (matrix/cofactor A 0 0)))
    (is (= 12 (matrix/cofactor A 0 1)))
    (is (= -46 (matrix/cofactor A 0 2)))
    (is (= -196 (matrix/determinant A)))))

(testing "Calculating the determinant of a 4x4 matrix"
  (let [A [[-2 -8  3  5]
           [-3  1  7  3]
           [ 1  2 -9  6]
           [-6  7  7 -9]]]
    (is (= 690 (matrix/cofactor A 0 0)))
    (is (= 447 (matrix/cofactor A 0 1)))
    (is (= 210 (matrix/cofactor A 0 2)))
    (is (= 51 (matrix/cofactor A 0 3)))
    (is (= -4071 (matrix/determinant A)))))

(testing "Testing an invertible matrix for invertibility"
  (let [A [[6  4 4  4]
           [5  5 7  6]
           [4 -9 3 -7]
           [9  1 7 -6]]]
    (is (= -2120 (matrix/determinant A)))
    (is (matrix/invertible? A))))

(testing "Testing a noninvertible matrix for invertibility"
  (let [A [[-4  2 -2 -3]
           [ 9  6  2  6]
           [ 0 -5  1 -5]
           [ 0  0  0  0]]]
    (is (zero? (matrix/determinant A)))
    (is (not (matrix/invertible? A)))))

(testing "Calculating the inverse of a matrix"
  (let [A [[-5 2 6 -8]
           [1 -5 1 8]
           [7 7 -6 -7]
           [1 -3 7 4]]
        B (matrix/inverse A)]
    (is (= 532 (matrix/determinant A)))
    (is (= -160 (matrix/cofactor A 2 3)))
    (is (= -160/532 (matrix/at B 3 2)))
    (is (= 105 (matrix/cofactor A 3 2)))
    (is (= 105/532 (matrix/at B 2 3)))
    (is (= [[ 0.21805  0.45113  0.24060 -0.04511]
            [-0.80827 -1.45677 -0.44361  0.52068]
            [-0.07895 -0.22368 -0.05263  0.19737]
            [-0.52256 -0.81391 -0.30075  0.30639]]
           (matrix/fmap (fn [x _ _] (->> x double (format "%.5f") Double.)) B)))))

(testing "Calculating the inverse of another matrix"
  (let [A [[8 -5 9 2]
           [7 5 6 1]
           [-6 0 9 6]
           [-3 0 -9 -4]]]
    (is (= [[-0.15385 -0.15385 -0.28205 -0.53846]
            [-0.07692  0.12308  0.02564  0.03077]
            [ 0.35897  0.35897  0.43590  0.92308]
            [-0.69231 -0.69231 -0.76923 -1.92308]]
           (matrix/fmap (fn [x _ _]
                          (->> x double (format "%.5f") Double.))
                        (matrix/inverse A))))))

(testing "Calculating the inverse of a third matrix"
  (let [A [[ 9  3  0  9]
           [-5 -2 -6 -3]
           [-4  9  6  4]
           [-7  6  6  2]]]
    (is (= [[-0.04074 -0.07778  0.14444 -0.22222]
            [-0.07778  0.03333  0.36667 -0.33333]
            [-0.02901 -0.14630 -0.10926  0.12963]
            [ 0.17778  0.06667 -0.26667  0.33333]]
           (matrix/fmap (fn [x _ _]
                          (->> x double (format "%.5f") Double.))
                        (matrix/inverse A))))))

(testing "Multiplying a product by its inverse"
  (let [A [[ 3 -9  7  3]
           [ 3 -8  2 -9]
           [-4  4  4  1]
           [-6  5 -1  1]]
        B [[8  2 2 2]
           [3 -1 7 0]
           [7  0 5 4]
           [6 -2 0 5]]
        C (matrix/multiply A B)]
    (is (= (matrix/multiply C (matrix/inverse B))
           A))))

(testing "Multiplying by a translation matrix"
  (let [transform (matrix/translation 5 -3 2)
        p (tuple/point -3 4 5)]
    (is (= (matrix/multiply transform p)
           (tuple/point 2 1 7)))))

(testing "Multiplying by the inverse of a translation matrix"
  (let [transform (matrix/translation 5 -3 2)
        inv (matrix/inverse transform)
        p (tuple/point -3 4 5)]
    (is (= (matrix/multiply inv p)
           (tuple/point -8 7 3)))))

(testing "Translation does not affect vectors"
  (let [transform (matrix/translation 5 -3 2)
        v (tuple/vector' -3 4 5)]
    (is (= (matrix/multiply transform v)
           v))))

(testing "A scaling matrix applied to a point"
  (let [transform (matrix/scaling 2 3 4)
        p (tuple/point -4 6 8)]
    (is (= (matrix/multiply transform p)
           (tuple/point -8 18 32)))))

(testing "A scaling matrix applied to a vector"
  (let [transform (matrix/scaling 2 3 4)
        v (tuple/vector' -4 6 8)]
    (is (= (matrix/multiply transform v)
           (tuple/vector' -8 18 32)))))

(testing "Multiplying by the inverse of a scaling matrix"
  (let [transform (matrix/scaling 2 3 4)
        inv (matrix/inverse transform)
        v (tuple/vector' -4 6 8)]
    (is (= (matrix/multiply inv v)
           (tuple/vector' -2 2 2)))))

(testing "Reflection is scaling by a negative value"
  (let [transform (matrix/scaling -1 1 1)
        p (tuple/point 2 3 4)]
    (is (= (matrix/multiply transform p)
           (tuple/point -2 3 4)))))

(testing "Rotating a point around the x axis"
  (let [p (tuple/point 0 1 0)
        half-quarter (matrix/rotation-x (/ pi 4))
        full-quarter (matrix/rotation-x (/ pi 2))]
    (is (matrix/eq (matrix/multiply half-quarter p)
                   (tuple/point 0
                                (/ (sqrt 2) 2)
                                (/ (sqrt 2) 2))))
    (is (matrix/eq (matrix/multiply full-quarter p)
                   (tuple/point 0 0 1)))))

(testing "The inverse of an x-rotation rotates in the opposite direction"
  (let [p (tuple/point 0 1 0)
        half-quarter (matrix/rotation-x (/ pi 4))
        inv (matrix/inverse half-quarter)]
    (is (matrix/eq (matrix/multiply inv p)
                   (tuple/point 0
                                (/ (sqrt 2) 2)
                                (- (/ (sqrt 2) 2)))))))

(testing "Rotating a point around the y axis"
  (let [p (tuple/point 0 0 1)
        half-quarter (matrix/rotation-y (/ pi 4))
        full-quarter (matrix/rotation-y (/ pi 2))]
    (is (matrix/eq (matrix/multiply half-quarter p)
                   (tuple/point (/ (sqrt 2) 2)
                                0
                                (/ (sqrt 2) 2))))
    (is (matrix/eq (matrix/multiply full-quarter p)
                   (tuple/point 1 0 0)))))

(testing "Rotating a point around the z axis"
  (let [p (tuple/point 0 1 0)
        half-quarter (matrix/rotation-z (/ pi 4))
        full-quarter (matrix/rotation-z (/ pi 2))]
    (is (matrix/eq (matrix/multiply half-quarter p)
                   (tuple/point (- (/ (sqrt 2) 2))
                                (/ (sqrt 2) 2)
                                0)))
    (is (matrix/eq (matrix/multiply full-quarter p)
                   (tuple/point -1 0 0)))))

(testing "A shearing transformation moves x in proportion to y"
  (let [transform (matrix/shearing 1 0 0 0 0 0)
        p (tuple/point 2 3 4)]
    (is (matrix/eq (matrix/multiply transform p)
                   (tuple/point 5 3 4)))))

(testing "A shearing transformation moves x in proportion to z"
  (let [transform (matrix/shearing 0 1 0 0 0 0)
        p (tuple/point 2 3 4)]
    (is (matrix/eq (matrix/multiply transform p)
                   (tuple/point 6 3 4)))))

(testing "A shearing transformation moves y in proportion to x"
  (let [transform (matrix/shearing 0 0 1 0 0 0)
        p (tuple/point 2 3 4)]
    (is (matrix/eq (matrix/multiply transform p)
                   (tuple/point 2 5 4)))))

(testing "A shearing transformation moves y in proportion to z"
  (let [transform (matrix/shearing 0 0 0 1 0 0)
        p (tuple/point 2 3 4)]
    (is (matrix/eq (matrix/multiply transform p)
                   (tuple/point 2 7 4)))))

(testing "A shearing transformation moves z in proportion to x"
  (let [transform (matrix/shearing 0 0 0 0 1 0)
        p (tuple/point 2 3 4)]
    (is (matrix/eq (matrix/multiply transform p)
                   (tuple/point 2 3 6)))))

(testing "A shearing transformation moves z in proportion to y"
  (let [transform (matrix/shearing 0 0 0 0 0 1)
        p (tuple/point 2 3 4)]
    (is (matrix/eq (matrix/multiply transform p)
                   (tuple/point 2 3 7)))))

(testing "Individual transformations are applied in sequence"
  (let [p (tuple/point 1 0 1)
        A (matrix/rotation-x (/ pi 2))
        B (matrix/scaling 5 5 5)
        C (matrix/translation 10 5 7)
        p2 (matrix/multiply A p)
        p3 (matrix/multiply B p2)
        p4 (matrix/multiply C p3)]
    (is (tuple/eq p2 (tuple/point 1 -1 0)))
    (is (tuple/eq p3 (tuple/point 5 -5 0)))
    (is (tuple/eq p4 (tuple/point 15 0 7)))))

(testing "Chained transformations must be applied in reverse order"
  (let [p (tuple/point 1 0 1)
        A (matrix/rotation-x (/ pi 2))
        B (matrix/scaling 5 5 5)
        C (matrix/translation 10 5 7)
        T (reduce matrix/multiply [C B A])]
    (is (tuple/eq (matrix/multiply T p)
                  (tuple/point 15 0 7)))))

(testing "Creating and querying a ray"
  (let [origin (tuple/point 1 2 3)
        direction (tuple/vector' 4 5 6)
        r (->ray origin direction)]
    (is (= (:origin r) origin))
    (is (= (:direction r) direction))))

(testing "Computing a point from a distance"
  (let [r (->ray (tuple/point 2 3 4) (tuple/vector' 1 0 0))]
    (is (= (position r 0) (tuple/point 2 3 4)))
    (is (= (position r 1) (tuple/point 3 3 4)))
    (is (= (position r -1) (tuple/point 1 3 4)))
    (is (= (position r 2.5) (tuple/point 4.5 3 4)))))

(testing "A ray intersects a sphere at two points"
  (let [r (->ray (tuple/point 0 0 -5) (tuple/vector' 0 0 1))
        s (sphere)
        xs (intersect s r)]
    (is (= xs [4.0 6.0]))))

(testing "A ray intersects a sphere at a tangent"
  (let [r (->ray (tuple/point 0 1 -5) (tuple/vector' 0 0 1))
        s (sphere)
        xs (intersect s r)]
    (is (= xs [5.0 5.0]))))

(testing "A ray misses a sphere"
  (let [r (->ray (tuple/point 0 2 -5) (tuple/vector' 0 0 1))
        s (sphere)
        xs (intersect s r)]
    (is (= xs []))))

(testing "A ray originates inside a sphere"
  (let [r (->ray (tuple/point 0 0 0) (tuple/vector' 0 0 1))
        s (sphere)
        xs (intersect s r)]
    (is (= xs [-1.0 1.0]))))

(testing "A sphere is behind a ray"
  (let [r (->ray (tuple/point 0 0 5) (tuple/vector' 0 0 1))
        s (sphere)
        xs (intersect s r)]
    (is (= xs [-6.0 -4.0]))))

(testing "An intersection encapsulates t and object"
  (let [s (sphere)
        i (intersection 3.5 s)]
    (is (= (:t i) 3.5))
    (is (= (:object i) s))))

)
